package io.chrisdavenport.mapref

import cats.syntax.all._
import cats.data._
import cats.effect.kernel._
import java.util.concurrent.atomic.AtomicBoolean

trait MapRefCompanionPlatform {
  
  def inScalaConcurrentTrieMap[G[_]: Sync, F[_]: Concurrent, K, V]: G[MapRef[F, K, Option[V]]] = 
    Sync[G].delay(scala.collection.concurrent.TrieMap.empty[K,V])
      .map(fromScalaConcurrentMap[F, K, V](_))

  def ofScalaConcurrentTrieMap[F[_]: Concurrent, K, V]: F[MapRef[F, K, Option[V]]] = 
    Concurrent[F].unit.map(_ => scala.collection.concurrent.TrieMap.empty[K,V])
      .map(fromScalaConcurrentMap[F, K, V](_))

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



}