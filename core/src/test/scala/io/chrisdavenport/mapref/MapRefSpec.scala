package io.chrisdavenport.mapref

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import cats._
import cats.implicits._
import cats.data.State
import cats.effect._
import cats.effect.concurrent.Ref

import cats.effect.specs2.CatsIO
import scala.concurrent.duration._

class MapRefSpec extends Specification with ScalaCheck with CatsIO {
  private val smallDelay: IO[Unit] = Timer[IO].sleep(20.millis)
  private def awaitEqual[A: Eq](t: IO[A], success: A): IO[Unit] =
      t.flatMap(a => if (Eq[A].eqv(a, success)) IO.unit else smallDelay *> awaitEqual(t, success))
  
  "MapRef.ofSingleImmutableMapRef" should {

    "concurrent modifications" in {
      val finalValue = 100
      val r = MapRef.ofSingleImmutableMap[IO, Unit, Int]().unsafeRunSync()
      val modifies = List.fill(finalValue)(IO.shift *> r(()).update(_.map(_ + 1))).parSequence
      val test = IO.shift *> r(()).set(Some(0)) *> modifies.start *> awaitEqual(r(()).get, finalValue.some)
      test.map(_ => ok)
    }

    "getAndSet - successful" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        getAndSetResult <- r(()).getAndSet(Some(1))
        getResult <- r(()).get
      } yield getAndSetResult == Some(0) && getResult == Some(1)

      op.map(_ must_=== true)
    }

    "access - successful" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter) <- r(()).access
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield success && result == Some(1)

      op.map(_ must_=== true)
    }

    "access - setter should fail if value is modified before setter is called with None/Some" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        (value, setter) <- r(()).access
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(_ must_=== true)
    }

    "access - setter should fail if value is modified before setter is called with init Some/Some" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter) <- r(()).access
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(_ must_=== true)
    }

    "access - setter should fail if value is modified before setter is called with init Some/None" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter) <- r(()).access
        _ <- r(()).set(Some(5))
        success <- setter(None)
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(_ must_=== true)
    }

    "access - setter should fail if called twice" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter)  <- r(()).access
        cond1 <- setter(value.map(_ + 1))
        _ <- r(()).set(value)
        cond2 <- setter(None)
        result <- r(()).get
      } yield cond1 && !cond2 && result == Some(0)

      op.map(_ must_=== true)
    }

    "tryUpdate - modification occurs successfully" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryUpdate(_.map(_+ 1))
        value <- r(()).get
      } yield result && value == Some(1)

      op.map(_ must_=== true)
    }

    "tryUpdate - should fail to update if modification has occurred" in {
      val updateRefUnsafely: Ref[IO, Option[Int]] => Unit = _.update(_.map(_ + 1)).unsafeRunSync()

      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryUpdate(
          currentValue => {
            updateRefUnsafely(r(()))
            currentValue.map(_ + 1)
          }
        )
      } yield result

      op.map(_ should_=== false)
    }

    "tryModifyState - modification occurs successfully" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryModifyState(State.pure(Some(1)))
      } yield result.contains(Some(1))

      op.map(_ should_=== true)
    }

    "modifyState - modification occurs successfully" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).modifyState(State.pure(Some(1)))
      } yield result == Some(1)

      op.map(_ must_=== true)
    }

    "Keys - empty" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        result <- r.keys
      } yield result

      op.map(_ must_=== Nil)
    }

    "keys - present" in {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Int, Int]()
        _ <- r(1).set(Some(1))
        result <- r.keys
      } yield result

      op.map(_ must_=== List(1))
    }
  }
  
  "MapRef.ofShardedImmutableMapRef" should {
    "return an updated value" in {// (shardCount: Int, key: Int) =>
      val size = 10
      val key = 3
      val test = for {
        map <- MapRef.ofShardedImmutableMap[IO, Int, String](size)
        _ <- map(key).set("Foo".some)
        out <- map(key).get
      } yield out

      test.map(_ must_=== Some("Foo"))
    }

    "work with convenience ops" in {
        import io.chrisdavenport.mapref.implicits._
        val size = 10
        val key = 3
        val expect = "Foo"
        val test = for {
          map <- MapRef.ofShardedImmutableMap[IO, Int, String](size)
          _ <- map.setKeyValue(key, expect)
          out <- map(key).get
        } yield out

        test.map(_ must_=== Some(expect))
    }

    "Keys - empty" in {
      val op = for {
        r <- MapRef.ofShardedImmutableMap[IO, Unit, Int](10)
        result <- r.keys
      } yield result

      op.map(_ must_=== Nil)
    }

    "keys - present" in {
      val op = for {
        r <- MapRef.ofShardedImmutableMap[IO, Int, Int](10)
        _ <- r(1).set(Some(1))
        _ <- r(2).set(Some(2))
        result <- r.keys
      } yield result

      op.map(_ must_=== List(1,2))
    }
  }

  "MapRef.ofConcurrentHashMap" should {
    "concurrent modifications" in {
      val finalValue = 100
      val r = MapRef.ofConcurrentHashMap[IO, Unit, Int]().unsafeRunSync()
      val modifies = List.fill(finalValue)(IO.shift *> r(()).update(_.map(_ + 1))).parSequence
      val test = IO.shift *> r(()).set(Some(0)) *> modifies.start *> awaitEqual(r(()).get, finalValue.some)
      test.map(_ => ok)
    }

    "getAndSet - successful" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        getAndSetResult <- r(()).getAndSet(Some(1))
        getResult <- r(()).get
      } yield getAndSetResult == Some(0) && getResult == Some(1)

      op.map(_ must_=== true)
    }

    "access - successful" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter) <- r(()).access
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield success && result == Some(1)

      op.map(_ must_=== true)
    }

    "access - setter should fail if value is modified before setter is called with None/Some" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        (value, setter) <- r(()).access
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(_ must_=== true)
    }

    "access - setter should fail if value is modified before setter is called with init Some/Some" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter) <- r(()).access
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(_ must_=== true)
    }

    "access - setter should fail if value is modified before setter is called with init Some/None" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter) <- r(()).access
        _ <- r(()).set(Some(5))
        success <- setter(None)
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(_ must_=== true)
    }

    "access - setter should fail if called twice" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        (value, setter)  <- r(()).access
        cond1 <- setter(value.map(_ + 1))
        _ <- r(()).set(value)
        cond2 <- setter(None)
        result <- r(()).get
      } yield cond1 && !cond2 && result == Some(0)

      op.map(_ must_=== true)
    }

    "tryUpdate - modification occurs successfully" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryUpdate(_.map(_+ 1))
        value <- r(()).get
      } yield result && value == Some(1)

      op.map(_ must_=== true)
    }

    "tryUpdate - should fail to update if modification has occurred" in {
      val updateRefUnsafely: Ref[IO, Option[Int]] => Unit = _.update(_.map(_ + 1)).unsafeRunSync()

      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryUpdate(
          currentValue => {
            updateRefUnsafely(r(()))
            currentValue.map(_ + 1)
          }
        )
      } yield result

      op.map(_ should_=== false)
    }

    "tryModifyState - modification occurs successfully" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryModifyState(State.pure(Some(1)))
      } yield result.contains(Some(1))

      op.map(_ should_=== true)
    }

    "modifyState - modification occurs successfully" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).modifyState(State.pure(Some(1)))
      } yield result == Some(1)

      op.map(_ must_=== true)
    }

    "Keys - empty" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        result <- r.keys
      } yield result

      op.map(_ must_=== Nil)
    }

    "keys - present" in {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Int, Int]()
        _ <- r(1).set(Some(1))
        result <- r.keys
      } yield result

      op.map(_ must_=== List(1))
    }


  }

}