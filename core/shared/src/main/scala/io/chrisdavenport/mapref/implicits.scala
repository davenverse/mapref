package io.chrisdavenport.mapref

import cats.syntax.all._

object implicits {
  implicit class MapRefOptionSyntax[F[_], K, V](private val mRef: MapRef[F, K, Option[V]]){
    def unsetKey(k: K): F[Unit] =
      mRef(k).set(None)
    def setKeyValue(k: K, v: V): F[Unit] = 
      mRef(k).set(v.some)
    def getAndSetKeyValue(k: K, v: V): F[Option[V]] = 
      mRef(k).getAndSet(v.some)

    def updateKeyValueIfSet(k: K, f: V => V): F[Unit] = 
      mRef(k).update{
        case None => None
        case Some(v) => f(v).some
      }

    def modifyKeyValueIfSet[B](k: K, f: V => (V, B)): F[Option[B]] =
      mRef(k).modify {
        case None => (None, None)
        case Some(v) => 
          val (set, out) = f(v)
          (set.some, out.some)
      }
  }
}