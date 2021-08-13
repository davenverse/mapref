package io.chrisdavenport.mapref

import cats.syntax.all._
import cats.effect.kernel._

trait MapRefCompanionPlatform {
  
  def inScalaConcurrentTrieMap[G[_]: Sync, F[_]: Concurrent, K, V]: G[MapRef[F, K, Option[V]]] = 
    Sync[G].delay(scala.collection.concurrent.TrieMap.empty[K,V])
      .map(MapRef.fromScalaConcurrentMap[F, K, V](_))

  def ofScalaConcurrentTrieMap[F[_]: Concurrent, K, V]: F[MapRef[F, K, Option[V]]] = 
    Concurrent[F].unit.map(_ => scala.collection.concurrent.TrieMap.empty[K,V])
      .map(MapRef.fromScalaConcurrentMap[F, K, V](_))

}