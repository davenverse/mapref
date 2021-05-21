package io.chrisdavenport.mapref

import cats.effect.kernel.Sync
import cats.implicits._


private[mapref] trait MapRefCompanionPlatform {

  def inScalaConcurrentTrieMap[G[_]: Sync, F[_]: Sync, K, V]: G[MapRef[F, K, Option[V]]] =
    Sync[G].delay(scala.collection.concurrent.TrieMap.empty[K,V])
      .map(MapRef.fromScalaConcurrentMap[F, K, V](_))

  def ofScalaConcurrentTrieMap[F[_]: Sync, K, V]: F[MapRef[F, K, Option[V]]] =
    inScalaConcurrentTrieMap[F, F,K, V]
}
