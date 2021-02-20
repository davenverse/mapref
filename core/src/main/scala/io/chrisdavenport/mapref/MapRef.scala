package io.chrisdavenport.mapref

import cats._
import cats.syntax.all._
import cats.data._
import cats.effect.kernel.{Ref, Sync}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable

/**
 * This is a total Map from K to Ref[F, V].
 * this allows us to use the Ref api backed by a ConcurrentHashMap
 *
 * This uses java universal hashCode and equality on K
 */
trait MapRef[F[_], K, V] extends Function1[K, Ref[F, V]]{
  /**
   * Access the reference for this Key
   */
  def apply(k: K): Ref[F, V]

  /** 
   * Snapshot of keys, may be concurrently updated after this snapshot
   * Experimental: Original interface was only apply, currently all
   * backends can implement this smoothly and its useful for operations you
   * need to do things to all the References.
   **/
  def keys: F[List[K]]
}

object MapRef  {

  private class ShardedImmutableMapImpl[F[_]: Sync, K, V](
    ref: K => Ref[F, Map[K, V]],
    val keys: F[List[K]]
  ) extends MapRef[F, K, Option[V]]{
    class HandleRef(k: K) extends Ref[F, Option[V]] {

      def access: F[(Option[V], Option[V] => F[Boolean])] = for {
        hasBeenCalled <- Sync[F].delay(new AtomicBoolean(false))
        thisRef = ref(k)
        current <- thisRef.get
      } yield {
        val checkCalled = Sync[F].delay(hasBeenCalled.compareAndSet(false, true))
        def endSet(f: Option[V] => F[Boolean]): Option[V] => F[Boolean] = {opt => 
          checkCalled
            .ifM(f(opt), Applicative[F].pure(false))
        }
        current.get(k) match {
          case None =>
            val set: Option[V] => F[Boolean] = { opt: Option[V] =>
              opt match {
                case None => thisRef.get.map(!_.isDefinedAt(k))
                case Some(newV) =>
                    thisRef.modify(map => 
                      if (!map.isDefinedAt(k)) (map + (k -> newV), true)
                      else (map, false)
                    )
              }
            }
            (None, set)
          case s@Some(_) => 
            val set: Option[V] => F[Boolean] = { opt => 
              opt match {
                case None => thisRef.modify(map => 
                  if (map.get(k) == s) {
                    (map - k, true)
                  } else {
                    (map, false)
                  }
                )
                case Some(value) => 
                  thisRef.modify(map => 
                    if (map.get(k) == s) {
                      (map + (k -> value), true)
                    } else {
                      (map, false)
                    }
                  )
              }
            }
            (s, endSet(set))
        }
      }

      def get: F[Option[V]] = ref(k).get.map(_.get(k))

      override def getAndSet(a: Option[V]): F[Option[V]] = 
        a match {
          case None => ref(k).modify(map => 
            (map -k, map.get(k))
          )
          case Some(v) => ref(k).modify(map => 
            (map + (k -> v), map.get(k))
          )
        }
      def modify[B](f: Option[V] => (Option[V], B)): F[B] = 
        ref(k).modify{map =>
          val current = map.get(k)
          val (setTo, out) = f(current)
          val finalMap = setTo match {
            case None => map - k
            case Some(v) => map + (k -> v)
          }
          (finalMap, out)
        }
      def modifyState[B](state: State[Option[V],B]): F[B] = 
        modify(state.run(_).value)
      def set(a: Option[V]): F[Unit] = {
        ref(k).update(m => 
          a match {
            case None => m - k
            case Some(v) => m + (k -> v)
          }
        )
      }
      def tryModify[B](f: Option[V] => (Option[V], B)): F[Option[B]] = {
        ref(k).tryModify{map =>
          val current = map.get(k)
          val (setTo, out) = f(current)
          val finalMap = setTo match {
            case None => map - k
            case Some(v) => map + (k -> v)
          }
          (finalMap, out)
        }
      }
      def tryModifyState[B](state: State[Option[V],B]): F[Option[B]] = 
        tryModify(state.run(_).value)
      def tryUpdate(f: Option[V] => Option[V]): F[Boolean] = {
        ref(k).tryUpdate(m => 
          f(m.get(k)) match {
            case None => m - k
            case Some(v) => m + (k -> v)
          }
        )
      }
      def update(f: Option[V] => Option[V]): F[Unit] = {
        ref(k).update(m => 
          f(m.get(k)) match {
            case None => m - k
            case Some(v) => m + (k -> v)
          }
        )
      }
    }

    def apply(k: K): Ref[F,Option[V]] = new HandleRef(k)
  }

/**
   * Creates a sharded map ref to reduce atomic contention on the Map,
   * given an efficient and equally distributed Hash, the contention
   * should allow for interaction like a general datastructure.
   */
  def ofShardedImmutableMap[ F[_]: Sync, K, V](
    shardCount: Int
  ): F[MapRef[F, K, Option[V]]]  = 
    inShardedImmutableMap[F, F, K, V](shardCount)

  /**
   * Creates a sharded map ref to reduce atomic contention on the Map,
   * given an efficient and equally distributed Hash, the contention
   * should allow for interaction like a general datastructure. Created in G, operates in F.
   */
  def inShardedImmutableMap[G[_]: Sync, F[_]: Sync, K, V](
    shardCount: Int
  ): G[MapRef[F, K, Option[V]]] = Sync[G].defer {
    assert(shardCount >= 1, "MapRef.sharded should have at least 1 shard")
    List.fill(shardCount)(())
      .traverse(_ => Ref.in[G, F, Map[K, V]](Map.empty))
      .map(_.toArray)
      .map{ array => 
        val refFunction = {k: K => 
          val location = Math.abs(k.## % shardCount)
          array(location)
        }
        val keys = array.toList.traverse(ref => ref.get.map(_.keys.toList)).map(_.flatten)
        new ShardedImmutableMapImpl[F, K, V](refFunction, keys)
      }
  }

  /**
   * Heavy Contention on Use
   */
  def ofSingleImmutableMap[F[_]: Sync, K, V](map: Map[K, V] = Map.empty[K, V]): F[MapRef[F, K, Option[V]]] = 
    inSingleImmutableMap[F, F, K, V](map)
  
  /**
   * Heavy Contention on Use. Created in G, operates in F.
   **/
  def inSingleImmutableMap[G[_]: Sync, F[_]: Sync, K, V](map: Map[K, V] = Map.empty[K, V]): G[MapRef[F, K, Option[V]]] = 
    Ref.in[G, F, Map[K, V]](map)
      .map(fromSingleImmutableMapRef[F, K, V])

  /**
   * Heavy Contention on Use, Allows you to access the underlying map through
   * processes outside of this interface. Useful for Atomic Map[K, V] => Map[K, V] interactions.
   **/
  def fromSingleImmutableMapRef[F[_]: Sync, K, V](ref: Ref[F, Map[K, V]]): MapRef[F, K, Option[V]] = 
    new ShardedImmutableMapImpl[F, K, V](_ => ref, ref.get.map(_.keys.toList))

  private class ConcurrentHashMapImpl[F[_], K, V](chm: ConcurrentHashMap[K, V], sync: Sync[F])
    extends MapRef[F, K, Option[V]] {
    private implicit def syncF: Sync[F] = sync

    val fnone0: F[None.type] = sync.pure(None)
    def fnone[A]: F[Option[A]] = fnone0.widen[Option[A]]

    class HandleRef(k: K) extends Ref[F, Option[V]] {

      def access: F[(Option[V], Option[V] => F[Boolean])] = 
        sync.delay {
          val hasBeenCalled = new AtomicBoolean(false)
          val init = chm.get(k)
          if (init == null) {
            val set: Option[V] => F[Boolean] = { opt: Option[V] =>
              opt match {
                case None => sync.delay(hasBeenCalled.compareAndSet(false, true) && !chm.containsKey(k))
                case Some(newV) =>
                  sync.delay {
                    // it was initially empty
                    hasBeenCalled.compareAndSet(false, true) && chm.putIfAbsent(k, newV) == null
                  }
              }
            }
            (None, set)
          } else {
            val set: Option[V] => F[Boolean] = { opt: Option[V] =>
              opt match {
                case None =>
                  sync.delay(hasBeenCalled.compareAndSet(false, true) && chm.remove(k, init))
                case Some(newV) =>
                  sync.delay(hasBeenCalled.compareAndSet(false, true) && chm.replace(k, init, newV))
              }
            }
            (Some(init), set)
          }
        }

      def get: F[Option[V]] =
        sync.delay {
          Option(chm.get(k))
        }

      override def getAndSet(a: Option[V]): F[Option[V]] =
        a match {
          case None =>
            sync.delay(Option(chm.remove(k)))
          case Some(v) =>
            sync.delay(Option(chm.put(k, v)))
        }

      def modify[B](f: Option[V] => (Option[V], B)): F[B] = {
        lazy val loop: F[B] = tryModify(f).flatMap {
          case None    => loop
          case Some(b) => sync.pure(b)
        }
        loop
      }

      def modifyState[B](state: State[Option[V], B]): F[B] =
        modify(state.run(_).value)

      def set(a: Option[V]): F[Unit] =
        a match {
          case None    => sync.delay { chm.remove(k); () }
          case Some(v) => sync.delay { chm.put(k, v); () }
        }

      def tryModify[B](f: Option[V] => (Option[V], B)): F[Option[B]] =
        // we need the suspend because we do effects inside
        sync.defer {
          val init = chm.get(k)
          if (init == null) {
            f(None) match {
              case (None, b) =>
                // no-op
                sync.pure(Some(b))
              case (Some(newV), b) =>
                if (chm.putIfAbsent(k, newV) == null) sync.pure(Some(b))
                else fnone
            }
          } else {
            f(Some(init)) match {
              case (None, b) =>
                if (chm.remove(k, init)) sync.pure(Some(b))
                else fnone
              case (Some(next), b) =>
                if (chm.replace(k, init, next)) sync.pure(Some(b))
                else fnone

            }
          }
        }

      def tryModifyState[B](state: State[Option[V], B]): F[Option[B]] =
        tryModify(state.run(_).value)

      def tryUpdate(f: Option[V] => Option[V]): F[Boolean] =
        tryModify { opt =>
          (f(opt), ())
        }.map(_.isDefined)

      def update(f: Option[V] => Option[V]): F[Unit] = {
        lazy val loop: F[Unit] = tryUpdate(f).flatMap {
          case true  => sync.unit
          case false => loop
        }
        loop
      }
    }

    val keys: F[List[K]] = sync.delay{
      val k = chm.keys()
      val builder = new mutable.ListBuffer[K]
      if (k != null){
        while (k.hasMoreElements()){
          val next = k.nextElement()
          builder.+=(next)
        }
      }
      builder.result()
    }

    def apply(k: K): Ref[F, Option[V]] = new HandleRef(k)
  }

  /**
   * Takes a ConcurrentHashMap, giving you access to the mutable state from the constructor.
   **/
  def fromConcurrentHashMap[F[_]: Sync, K, V](map: ConcurrentHashMap[K, V]): MapRef[F, K, Option[V]] = 
    new ConcurrentHashMapImpl[F, K, V](map, Sync[F])

  /**
   * This allocates mutable memory, so it has to be inside F. The way to use things like this is to
   * allocate one then `.map` them inside of constructors that need to access them.
   *
   * It is usually a mistake to have a `G[RefMap[F, K, V]]` field. You want `RefMap[F, K, V]` field
   * which means the thing that needs it will also have to be inside of `F[_]`, which is because
   * it needs access to mutable state so allocating it is also an effect.
   */
  def inConcurrentHashMap[G[_]: Sync, F[_]: Sync, K, V](
    initialCapacity: Int = 16,
    loadFactor: Float = 0.75f,
    concurrencyLevel: Int = 16
  ): G[MapRef[F, K, Option[V]]] =
    Sync[G].delay{
      fromConcurrentHashMap[F, K, V](new ConcurrentHashMap[K, V](initialCapacity, loadFactor, concurrencyLevel))
    }

  /**
   * This allocates mutable memory, so it has to be inside F. The way to use things like this is to
   * allocate one then `.map` them inside of constructors that need to access them.
   *
   * It is usually a mistake to have a `F[RefMap[F, K, V]]` field. You want `RefMap[F, K, V]` field
   * which means the thing that needs it will also have to be inside of `F[_]`, which is because
   * it needs access to mutable state so allocating it is also an effect.
   */
  def ofConcurrentHashMap[F[_]: Sync, K, V](
    initialCapacity: Int = 16,
    loadFactor: Float = 0.75f,
    concurrencyLevel: Int = 16
  ): F[MapRef[F, K, Option[V]]] = 
    inConcurrentHashMap[F, F, K, V](initialCapacity, loadFactor, concurrencyLevel)


  private class ScalaConcurrentMapImpl[F[_], K, V](map: scala.collection.concurrent.Map[K, V])(implicit sync: Sync[F])
    extends MapRef[F, K, Option[V]]{

    val fnone0: F[None.type] = sync.pure(None)
    def fnone[A]: F[Option[A]] = fnone0.widen[Option[A]]

      class HandleRef(k: K) extends Ref[F, Option[V]] {
        def access: F[(Option[V], Option[V] => F[Boolean])] = 
        sync.delay {
          val hasBeenCalled = new AtomicBoolean(false)
          val init = map.get(k) 
          init match {
            case None => 
              val set: Option[V] => F[Boolean] = { opt: Option[V] =>
                opt match {
                  case None => 
                    sync.delay(hasBeenCalled.compareAndSet(false, true) && !map.contains(k))
                  case Some(newV) =>
                    sync.delay {
                      // it was initially empty
                      hasBeenCalled.compareAndSet(false, true) && map.putIfAbsent(k, newV).isEmpty
                    }
                }
              }
              (None, set)
            case Some(old) => 
              val set: Option[V] => F[Boolean] = { opt: Option[V] =>
                opt match {
                  case None =>
                    sync.delay(hasBeenCalled.compareAndSet(false, true) && map.remove(k, old))
                  case Some(newV) =>
                    sync.delay(hasBeenCalled.compareAndSet(false, true) && map.replace(k, old, newV))
                }
              }
              (init, set)
          }
        }

      def get: F[Option[V]] =
        sync.delay {
          map.get(k)
        }

      override def getAndSet(a: Option[V]): F[Option[V]] =
        a match {
          case None =>
            sync.delay(map.remove(k))
          case Some(v) =>
            sync.delay(map.put(k, v))
        }

      def modify[B](f: Option[V] => (Option[V], B)): F[B] = {
        lazy val loop: F[B] = tryModify(f).flatMap {
          case None    => loop
          case Some(b) => sync.pure(b)
        }
        loop
      }

      def modifyState[B](state: State[Option[V], B]): F[B] =
        modify(state.run(_).value)

      def set(a: Option[V]): F[Unit] =
        a match {
          case None    => sync.delay { map.remove(k); () }
          case Some(v) => sync.delay { map.put(k, v); () }
        }

        def tryModify[B](f: Option[V] => (Option[V], B)): F[Option[B]] = // we need the suspend because we do effects inside
          sync.defer {
            val init = map.get(k)
            init match {
              case None => 
                f(None) match {
                  case (None, b) =>
                    // no-op
                    sync.pure(Some(b))
                  case (Some(newV), b) =>
                    sync.pure(map.putIfAbsent(k, newV).fold(b.some)(_ => None))
                }
              case Some(initV) => 
                f(init) match {
                  case (None, b) =>
                    if (map.remove(k, initV)) sync.pure(Some(b))
                    else fnone
                  case (Some(next), b) =>
                    if (map.replace(k, initV, next)) sync.pure(Some(b))
                    else fnone
                }
            }
          }

        def tryModifyState[B](state: State[Option[V], B]): F[Option[B]] =
          tryModify(state.run(_).value)
  
        def tryUpdate(f: Option[V] => Option[V]): F[Boolean] =
          tryModify { opt =>
            (f(opt), ())
          }.map(_.isDefined)
  
        def update(f: Option[V] => Option[V]): F[Unit] = {
          lazy val loop: F[Unit] = tryUpdate(f).flatMap {
            case true  => sync.unit
            case false => loop
          }
          loop
        }
      }

    /**
     * Access the reference for this Key
     */
    def apply(k: K): Ref[F, Option[V]] = new HandleRef(k)

    /** 
     * Snapshot of keys, may be concurrently updated after this snapshot
     * Experimental: Original interface was only apply, currently all
     * backends can implement this smoothly and its useful for operations you
     * need to do things to all the References.
     **/
    def keys: F[List[K]] = Sync[F].delay(map.keys.toList)
  }

  /**
   * Takes a scala.collection.conurrent.Map, giving you access to the mutable state from the constructor.
   **/
  def fromScalaConcurrentMap[F[_]: Sync, K, V](map: scala.collection.concurrent.Map[K, V]): MapRef[F, K, Option[V]] = 
    new ScalaConcurrentMapImpl[F, K, V](map)

  def inScalaConcurrentTrieMap[G[_]: Sync, F[_]: Sync, K, V]: G[MapRef[F, K, Option[V]]] = 
    Sync[G].delay(scala.collection.concurrent.TrieMap.empty[K,V])
      .map(fromScalaConcurrentMap[F, K, V](_))

  def ofScalaConcurrentTrieMap[F[_]: Sync, K, V]: F[MapRef[F, K, Option[V]]] = 
    inScalaConcurrentTrieMap[F, F,K, V]
}
