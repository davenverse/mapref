package io.chrisdavenport.mapref

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import cats.implicits._
import cats.effect.IO

class MapRefSpec extends Specification with ScalaCheck {
  "MapRef" should {
    "return an updated value" in {// (shardCount: Int, key: Int) =>
      val size = 10
      val key = 3
      val test = for {
        map <- MapRef.sharded[IO, Int, String](size)
        _ <- map.set("Foo".some).run(key)
        out <- map.get.run(key)
      } yield out

      test.unsafeRunSync must_=== Some("Foo")
    }

    "work with convenience ops" in {
      val size = 10
      val key = 3
      val test = for {
        map <- MapRef.sharded[IO, Int, String](size)
        _ <- map.setKeyValue(key, "Foo")
        out <- map.getKeyValue(key)
      } yield out

      test.unsafeRunSync must_=== Some("Foo")
    }
  }

}