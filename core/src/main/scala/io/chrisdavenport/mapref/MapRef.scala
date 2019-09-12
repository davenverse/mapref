package io.chrisdavenport.mapref

import cats._
import cats.implicits._
import cats.data._
import cats.kernel.Hash
import cats.effect.Sync
import cats.effect.concurrent.Ref

/**
 * Convenience Methods for Interaction with MapLike Ref Structures.
 * 
 * Everything here is defaulted. Because it can be inferred by the
 * underlying type. But honestly interacting with 
 * {{{RefLike[Kleisli[F, K, ?], Option[V] }}} is not a ton of fun.
 * So some convenience was in order to easy the interaction.
 */
trait MapRef[F[_], K, V] extends RefLite[Kleisli[F, K, ?], Option[V]]{
  
  def getKeyValue(k: K): F[Option[V]] =
    get.run(k)

  def setKeyValue(k: K, a: V): F[Unit] =
    set(a.some).run(k)

  def unsetKey(k: K): F[Unit] =
    set(None).run(k)

  def getAndSetKeyValue(k: K, a: V): F[Option[V]] = 
    getAndSet(a.some).run(k)

  def tryUpdateKeyValue(k: K, f: Option[V] => Option[V]): F[Boolean] = 
    tryUpdate(f).run(k)

  def tryModifyKeyValue[B](k: K, f: Option[V] => (Option[V], B)): F[Option[B]] = 
    tryModify(f).run(k)
    
  def updateKeyValueIfSet(k:K, f: V => V): F[Unit] = 
    updateKeyValue(k, {
      case None => None
      case Some(v) => f(v).some
    })

  def updateKeyValue(k: K, f: Option[V] => Option[V]): F[Unit] =
    update(f).run(k)

  def modifyKeyValueIfSet[B](k: K, f: V => (V, B)): F[Option[B]] =
    modifyKeyValue(k, {
      case None => (None, None)
      case Some(v) => 
        val (set, out) = f(v)
        (set.some, out.some)
    })

  def modifyKeyValue[B](k: K, f: Option[V] => (Option[V], B)): F[B] =
    modify(f).run(k)
}

object MapRef  {

  /**
   * Creates a sharded map ref to reduce atomic contention on the Map,
   * given an efficient and equally distributed Hash, the contention
   * should allow for interaction like a general datastructure.
   * 
   */
  def sharded[F[_]: Sync, K: Hash, V](
    shardCount: Int
  ): F[MapRef[F, K, V]] = Sync[F].suspend{
    assert(shardCount >= 1, "MapRef.sharded should have at least 1 shard")
    List.fill(shardCount)(())
      .traverse(_ => Ref.of[F, Map[K, V]](Map.empty))
      .map(_.toArray)
      .map{array => 
        val refFunction = {k: K => 
          val location = Math.abs(Hash[K].hash(k) % shardCount)
          // println(s"$k at $location")
          array(location)
        }
        new ShardedMapImpl[F, K, V](refFunction)
      }
  }

  /**
   * Doesn't require a Hashable Key
   */
  def single[F[_]: Sync, K, V]: F[MapRef[F, K, V]] = 
    Ref.of[F, Map[K, V]](Map.empty)
      .map(m => new ShardedMapImpl[F, K, V](_ => m))


  
  private class ShardedMapImpl[F[_], K, V](
    ref: K => Ref[F, Map[K, V]]
  ) extends MapRef[F, K, V]{
    
    def get: Kleisli[F,K,Option[V]] = Kleisli{ k => 
      ref(k).modify(m => (m, m.get(k)))
    }

    def getAndSet(a: Option[V]): Kleisli[F,K,Option[V]] = 
      Kleisli{k =>
        a match {
          case None => 
            ref(k).modify( map => 
              (map - k, map.get(k))
            )
          case Some(v) =>
            ref(k).modify( map => 
              (map + (k -> v), map.get(k))
            )
        }
      }
    def modify[B](f: Option[V] => (Option[V], B)): Kleisli[F,K,B] = 
    Kleisli{k => 
      ref(k).modify{map =>
        val current = map.get(k)
        val (setTo, out) = f(current)
        val finalMap = setTo match {
          case None => map - k
          case Some(v) => map + (k -> v)
        }
        (finalMap, out)
      }
    }

    def set(a: Option[V]): Kleisli[F,K,Unit] = 
      Kleisli{k => 
        ref(k).update(m => 
          a match {
            case None => m - k
            case Some(v) => m + (k -> v)
          }
        )
      }

    def tryModify[B](f: Option[V] => (Option[V], B)): Kleisli[F,K,Option[B]] = 
      Kleisli{ k => 
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

    def tryUpdate(f: Option[V] => Option[V]): Kleisli[F,K,Boolean] = 
      Kleisli{k => 
        ref(k).tryUpdate(m => 
          f(m.get(k)) match {
            case None => m - k
            case Some(v) => m + (k -> v)
          }
        )
      }
    def update(f: Option[V] => Option[V]): Kleisli[F,K,Unit] = 
      Kleisli{k => 
        ref(k).update(m => 
          f(m.get(k)) match {
            case None => m - k
            case Some(v) => m + (k -> v)
          }
        )
      }
  }

}