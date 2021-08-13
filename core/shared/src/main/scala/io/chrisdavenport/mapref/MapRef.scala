package io.chrisdavenport.mapref

import cats._
import cats.conversions.all._
import cats.syntax.all._
import cats.data._
import cats.effect.kernel._
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
}

object MapRef extends MapRefCompanionPlatform  {

  private class ShardedImmutableMapImpl[F[_]: Concurrent, K, V](
    ref: K => Ref[F, Map[K, V]],
    val keys: F[List[K]]
  ) extends MapRef[F, K, Option[V]]{
    class HandleRef(k: K) extends Ref[F, Option[V]] {

      def access: F[(Option[V], Option[V] => F[Boolean])] = for {
        hasBeenCalled <- Applicative[F].unit.map(_ => new AtomicBoolean(false))
        thisRef = ref(k)
        current <- thisRef.get
      } yield {
        val checkCalled = Applicative[F].unit.map(_ => hasBeenCalled.compareAndSet(false, true))
        def endSet(f: Option[V] => F[Boolean]): Option[V] => F[Boolean] = {opt => 
          checkCalled
            .ifM(f(opt), Applicative[F].pure(false))
        }
        current.get(k) match {
          case None =>
            val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
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
  def ofShardedImmutableMap[F[_]: Concurrent, K, V](
    shardCount: Int
  ): F[MapRef[F, K, Option[V]]]  = {
    assert(shardCount >= 1, "MapRef.sharded should have at least 1 shard")
    List.fill(shardCount)(())
      .traverse(_ => Concurrent[F].ref[Map[K, V]](Map.empty))
      .map(fromSeqRefs(_))
  }

  /**
   * Creates a sharded map ref to reduce atomic contention on the Map,
   * given an efficient and equally distributed Hash, the contention
   * should allow for interaction like a general datastructure. Created in G, operates in F.
   */
  def inShardedImmutableMap[G[_]: Sync, F[_]: Async, K, V](
    shardCount: Int
  ): G[MapRef[F, K, Option[V]]] = Sync[G].defer {
    assert(shardCount >= 1, "MapRef.sharded should have at least 1 shard")
    List.fill(shardCount)(())
      .traverse(_ => Ref.in[G, F, Map[K, V]](Map.empty))
      .map(fromSeqRefs(_))
  }

  def fromSeqRefs[F[_]: Concurrent, K, V](
    seq: scala.collection.immutable.Seq[Ref[F, Map[K, V]]]
  ): MapRef[F, K, Option[V]] = {
    val array = seq.toArray
    val shardCount = seq.size
    val refFunction = { (k: K) =>
    val location = Math.abs(k.## % shardCount)
      array(location)
    }
    val keys = array.toList.traverse(ref => ref.get.map(_.keys.toList)).map(_.flatten)
    new ShardedImmutableMapImpl[F, K, V](refFunction, keys)
  }

  /**
   * Heavy Contention on Use
   */
  def ofSingleImmutableMap[F[_]: Concurrent , K, V](map: Map[K, V] = Map.empty[K, V]): F[MapRef[F, K, Option[V]]] = 
    Concurrent[F].ref(map)
      .map(fromSingleImmutableMapRef[F, K, V](_))
  
  /**
   * Heavy Contention on Use. Created in G, operates in F.
   **/
  def inSingleImmutableMap[G[_]: Sync, F[_]: Async, K, V](map: Map[K, V] = Map.empty[K, V]): G[MapRef[F, K, Option[V]]] = 
    Ref.in[G, F, Map[K, V]](map)
      .map(fromSingleImmutableMapRef[F, K, V](_))

  /**
   * Heavy Contention on Use, Allows you to access the underlying map through
   * processes outside of this interface. Useful for Atomic Map[K, V] => Map[K, V] interactions.
   **/
  def fromSingleImmutableMapRef[F[_]: Concurrent, K, V](ref: Ref[F, Map[K, V]]): MapRef[F, K, Option[V]] = 
    new ShardedImmutableMapImpl[F, K, V](_ => ref, ref.get.map(_.keys.toList))


  private class ConcurrentHashMapImpl[F[_], K, V](chm: ConcurrentHashMap[K, V], concurrent: Concurrent[F])
    extends MapRef[F, K, Option[V]] {
    private implicit def concurrentF: Concurrent[F] = concurrent

    val fnone0: F[None.type] = concurrent.pure(None)
    def fnone[A]: F[Option[A]] = fnone0.widen[Option[A]]

    def delay[A](a: => A): F[A] = concurrent.unit.map(_ => a)

    class HandleRef(k: K) extends Ref[F, Option[V]] {

      def access: F[(Option[V], Option[V] => F[Boolean])] = 
        delay {
          val hasBeenCalled = new AtomicBoolean(false)
          val init = chm.get(k)
          if (init == null) {
            val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
              opt match {
                case None => delay(hasBeenCalled.compareAndSet(false, true) && !chm.containsKey(k))
                case Some(newV) =>
                  delay {
                    // it was initially empty
                    hasBeenCalled.compareAndSet(false, true) && chm.putIfAbsent(k, newV) == null
                  }
              }
            }
            (None, set)
          } else {
            val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
              opt match {
                case None =>
                  delay(hasBeenCalled.compareAndSet(false, true) && chm.remove(k, init))
                case Some(newV) =>
                  delay(hasBeenCalled.compareAndSet(false, true) && chm.replace(k, init, newV))
              }
            }
            (Some(init), set)
          }
        }

      def get: F[Option[V]] =
        delay {
          Option(chm.get(k))
        }

      override def getAndSet(a: Option[V]): F[Option[V]] =
        a match {
          case None =>
            delay(Option(chm.remove(k)))
          case Some(v) =>
            delay(Option(chm.put(k, v)))
        }

      def modify[B](f: Option[V] => (Option[V], B)): F[B] = {
        lazy val loop: F[B] = tryModify(f).flatMap {
          case None    => loop
          case Some(b) => concurrent.pure(b)
        }
        loop
      }

      def modifyState[B](state: State[Option[V], B]): F[B] =
        modify(state.run(_).value)

      def set(a: Option[V]): F[Unit] =
        a match {
          case None    => delay { chm.remove(k); () }
          case Some(v) => delay { chm.put(k, v); () }
        }

      def tryModify[B](f: Option[V] => (Option[V], B)): F[Option[B]] =
        // we need the suspend because we do effects inside
      delay {
          val init = chm.get(k)
          if (init == null) {
            f(None) match {
              case (None, b) =>
                // no-op
                concurrent.pure(b.some)
              case (Some(newV), b) =>
                if (chm.putIfAbsent(k, newV) == null) concurrent.pure(b.some)
                else fnone
            }
          } else {
            f(Some(init)) match {
              case (None, b) =>
                if (chm.remove(k, init)) concurrent.pure(Some(b))
                else fnone[B]
              case (Some(next), b) =>
                if (chm.replace(k, init, next)) concurrent.pure(Some(b))
                else fnone[B]

            }
          }
        }.flatten

      def tryModifyState[B](state: State[Option[V], B]): F[Option[B]] =
        tryModify(state.run(_).value)

      def tryUpdate(f: Option[V] => Option[V]): F[Boolean] =
        tryModify { opt =>
          (f(opt), ())
        }.map(_.isDefined)

      def update(f: Option[V] => Option[V]): F[Unit] = {
        lazy val loop: F[Unit] = tryUpdate(f).flatMap {
          case true  => concurrent.unit
          case false => loop
        }
        loop
      }
    }

    val keys: F[List[K]] = delay{
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
  def fromConcurrentHashMap[F[_]: Concurrent, K, V](map: ConcurrentHashMap[K, V]): MapRef[F, K, Option[V]] = 
    new ConcurrentHashMapImpl[F, K, V](map, Concurrent[F])

  /**
   * This allocates mutable memory, so it has to be inside F. The way to use things like this is to
   * allocate one then `.map` them inside of constructors that need to access them.
   *
   * It is usually a mistake to have a `G[RefMap[F, K, V]]` field. You want `RefMap[F, K, V]` field
   * which means the thing that needs it will also have to be inside of `F[_]`, which is because
   * it needs access to mutable state so allocating it is also an effect.
   */
  def inConcurrentHashMap[G[_]: Sync, F[_]: Concurrent, K, V](
    initialCapacity: Int = 16,
    loadFactor: Float = 0.75f,
    concurrencyLevel: Int = 16
  ): G[MapRef[F, K, Option[V]]] =
    Sync[G].delay(new ConcurrentHashMap[K, V](initialCapacity, loadFactor, concurrencyLevel))
      .map(fromConcurrentHashMap[F, K, V])

  /**
   * This allocates mutable memory, so it has to be inside F. The way to use things like this is to
   * allocate one then `.map` them inside of constructors that need to access them.
   *
   * It is usually a mistake to have a `F[RefMap[F, K, V]]` field. You want `RefMap[F, K, V]` field
   * which means the thing that needs it will also have to be inside of `F[_]`, which is because
   * it needs access to mutable state so allocating it is also an effect.
   */
  def ofConcurrentHashMap[F[_]: Concurrent, K, V](
    initialCapacity: Int = 16,
    loadFactor: Float = 0.75f,
    concurrencyLevel: Int = 16
  ): F[MapRef[F, K, Option[V]]] = 
    Concurrent[F].unit.map(_ => new ConcurrentHashMap[K, V](initialCapacity, loadFactor, concurrencyLevel))
      .map(fromConcurrentHashMap[F, K, V])


    /**
   * Takes a scala.collection.conurrent.Map, giving you access to the mutable state from the constructor.
   **/
  def fromScalaConcurrentMap[F[_]: Concurrent, K, V](map: scala.collection.concurrent.Map[K, V]): MapRef[F, K, Option[V]] = 
    new ScalaConcurrentMapImpl[F, K, V](map)


  private class ScalaConcurrentMapImpl[F[_], K, V](map: scala.collection.concurrent.Map[K, V])(implicit concurrent: Concurrent[F])
    extends MapRef[F, K, Option[V]]{

    val fnone0: F[None.type] = concurrent.pure(None)
    def fnone[A]: F[Option[A]] = fnone0.widen[Option[A]]

    class HandleRef(k: K) extends Ref[F, Option[V]] {
      def access: F[(Option[V], Option[V] => F[Boolean])] =
      concurrent.unit.map {_ => 
        val hasBeenCalled = new AtomicBoolean(false)
        val init = map.get(k)
        init match {
          case None =>
            val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
              opt match {
                case None =>
                concurrent.unit.map(_=> hasBeenCalled.compareAndSet(false, true) && !map.contains(k))
                case Some(newV) =>
                  concurrent.unit.map {_ => 
                    // it was initially empty
                    hasBeenCalled.compareAndSet(false, true) && map.putIfAbsent(k, newV).isEmpty
                  }
              }
            }
            (None, set)
          case Some(old) =>
            val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
              opt match {
                case None =>
                  concurrent.unit.map(_=> hasBeenCalled.compareAndSet(false, true) && map.remove(k, old))
                case Some(newV) =>
                  concurrent.unit.map(_=> hasBeenCalled.compareAndSet(false, true) && map.replace(k, old, newV))
              }
            }
            (init, set)
        }
      }

      def get: F[Option[V]] =
        concurrent.unit.map(_=> 
          map.get(k)
        )

      override def getAndSet(a: Option[V]): F[Option[V]] =
        a match {
          case None =>
            concurrent.unit.map(_=> map.remove(k))
          case Some(v) =>
            concurrent.unit.map(_=> map.put(k, v))
        }

      def modify[B](f: Option[V] => (Option[V], B)): F[B] = {
        lazy val loop: F[B] = tryModify(f).flatMap {
          case None    => loop
          case Some(b) => concurrent.pure(b)
        }
        loop
      }

      def modifyState[B](state: State[Option[V], B]): F[B] =
        modify(state.run(_).value)

      def set(a: Option[V]): F[Unit] =
        a match {
          case None    => concurrent.unit.map{_=>  map.remove(k); () }
          case Some(v) => concurrent.unit.map{_=>  map.put(k, v); () }
        }

      def tryModify[B](f: Option[V] => (Option[V], B)): F[Option[B]] = // we need the suspend because we do effects inside
        concurrent.unit.map{_=> 
          val init = map.get(k)
          init match {
            case None =>
              f(None) match {
                case (None, b) =>
                  // no-op
                  concurrent.pure(b.some)
                case (Some(newV), b) =>
                  concurrent.unit.map(_ =>map.putIfAbsent(k, newV).fold[Option[B]](b.some)(_ => None))
              }
            case Some(initV) =>
              f(init) match {
                case (None, b) =>
                  if (map.remove(k, initV)) concurrent.pure(b.some)
                  else fnone[B]
                case (Some(next), b) =>
                  if (map.replace(k, initV, next)) concurrent.pure(b.some)
                  else fnone[B]
              }
          }
        }.flatten

      def tryModifyState[B](state: State[Option[V], B]): F[Option[B]] =
        tryModify(state.run(_).value)
  
      def tryUpdate(f: Option[V] => Option[V]): F[Boolean] =
        tryModify { opt =>
          (f(opt), ())
        }.map(_.isDefined)
  
      def update(f: Option[V] => Option[V]): F[Unit] = {
        lazy val loop: F[Unit] = tryUpdate(f).flatMap {
          case true  => concurrent.unit
          case false => loop
        }
        loop
      }
    }

    /**
     * Access the reference for this Key
     */
    def apply(k: K): Ref[F, Option[V]] = new HandleRef(k)

  }


  implicit def mapRefInvariant[F[_]: Functor, K]: Invariant[MapRef[F, K, *]] =
    new MapRefInvariant[F, K]

  private[mapref] class MapRefInvariant[F[_]: Functor, K] extends Invariant[MapRef[F, K, *]] {
    override def imap[V, V0](fa: MapRef[F, K, V])(f: V => V0)(g: V0 => V): MapRef[F, K, V0] =
      new MapRef[F, K, V0] {

        override def apply(k: K): Ref[F, V0] = fa(k).imap(f)(g)
      }
  }
}
