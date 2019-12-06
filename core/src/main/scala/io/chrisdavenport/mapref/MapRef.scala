package io.chrisdavenport.mapref

import cats._
import cats.implicits._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

/**
 * This is a total Map from K to Ref[F, V].
 * this allows us to use the Ref api backed by a ConcurrentHashMap
 *
 * This uses java universal hashCode and equality on K
 */
trait MapRef[F[_], K, V] {
  def apply(k: K): Ref[F, V]
}

object MapRef  {

  private class SimpleMapRef[F[_], K, V](f: K => Ref[F, V]) extends MapRef[F, K, V]{
    def apply(k: K): Ref[F, V] = f(k)
  }

  /**
   * Ease of Use Constructor to Access MapRef
   **/
  def simple[F[_], K, V](f: K => Ref[F, V]): MapRef[F, K, V] = 
    new SimpleMapRef[F, K, V](f)

  private class ShardedImmutableMapImpl[F[_]: Sync, K, V](
    ref: K => Ref[F, Map[K, V]]
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

      def get: F[Option[V]] = ref(k).modify(m => (m, m.get(k)))

      def getAndSet(a: Option[V]): F[Option[V]] = 
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
  def fromShardedImmutableMapRef[F[_]: Sync, K, V](
    shardCount: Int
  ): F[MapRef[F, K, Option[V]]] = Sync[F].suspend{
    assert(shardCount >= 1, "MapRef.sharded should have at least 1 shard")
    List.fill(shardCount)(())
      .traverse(_ => Ref.of[F, Map[K, V]](Map.empty))
      .map(_.toArray)
      .map{array => 
        val refFunction = {k: K => 
          val location = Math.abs(k.## % shardCount)
          array(location)
        }
        new ShardedImmutableMapImpl[F, K, V](refFunction)
      }
  }

  /**
   * Heavy Contention on Use
   */
  def fromSingleImmutableMap[F[_]: Sync, K, V](map: Map[K, V] = Map.empty[K, V]): F[MapRef[F, K, Option[V]]] = 
    Ref.of[F, Map[K, V]](map)
      .map(fromSingleImmutableMapRef[F, K, V])

  /**
   * Heavy Contention on Use, Allows you to access the underlying map through
   * processes outside of this interface. Useful for Atomic Map[K, V] => Map[K, V] interactions.
   **/
  def fromSingleImmutableMapRef[F[_]: Sync, K, V](ref: Ref[F, Map[K, V]]): MapRef[F, K, Option[V]] = 
    new ShardedImmutableMapImpl[F, K, V](_ => ref)

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

      def getAndSet(a: Option[V]): F[Option[V]] =
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
        sync.suspend {
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

    def apply(k: K): Ref[F, Option[V]] = new HandleRef(k)
  }

  /**
   * This allocates mutable memory, so it has to be inside F. The way to use things like this is to
   * allocate one then `.map` them inside of constructors that need to access them.
   *
   * It is usually a mistake to have a `F[RefMap[F, K, V]]` field. You want `RefMap[F, K, V]` field
   * which means the thing that needs it will also have to be inside of `F[_]`, which is because
   * it needs access to mutable state so allocating it is also an effect.
   */
  def fromConcurrentHashMap[F[_]: Sync, K, V](
    initialCapacity: Int = 16,
    loadFactor: Float = 0.75f,
    concurrencyLevel: Int = 16
  ): F[MapRef[F, K, Option[V]]] =
    Sync[F].delay(
      new ConcurrentHashMapImpl[F, K, V](new ConcurrentHashMap[K, V](initialCapacity, loadFactor, concurrencyLevel),
                        Sync[F]))


}