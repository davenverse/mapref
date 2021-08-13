package io.chrisdavenport.mapref

import cats._
import cats.implicits._
import cats.data.State
import cats.effect._

import scala.concurrent.duration._

class MapRefSpec extends munit.CatsEffectSuite {
  private val smallDelay: IO[Unit] = IO.sleep(20.millis)
  private def awaitEqual[A: Eq](t: IO[A], success: A): IO[Unit] =
      t.flatMap(a => if (Eq[A].eqv(a, success)) IO.unit else smallDelay *> awaitEqual(t, success))
  
    test("MapRef.ofSingleImmutableMapRef - concurrent modifications") {
      val finalValue = 100
      val r = MapRef.inSingleImmutableMap[SyncIO, IO, Unit, Int]().unsafeRunSync()
      val modifies = List.fill(finalValue)(r(()).update(_.map(_ + 1))).parSequence
      val test = r(()).set(Some(0)) *> modifies.start *> awaitEqual(r(()).get, finalValue.some)
      test.map(_ => assert(true))
    }

    test("MapRef.ofSingleImmutableMapRef - getAndSet - successful") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        getAndSetResult <- r(()).getAndSet(Some(1))
        getResult <- r(()).get
      } yield getAndSetResult == Some(0) && getResult == Some(1)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - access - successful") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield success && result == Some(1)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - access - setter should fail if value is modified before setter is called with None/Some") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        accessed <- r(()).access
        (value, setter) = accessed
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - access - setter should fail if value is modified before setter is called with init Some/Some") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - access - setter should fail if value is modified before setter is called with init Some/None") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        _ <- r(()).set(Some(5))
        success <- setter(None)
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - access - setter should fail if called twice") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        cond1 <- setter(value.map(_ + 1))
        _ <- r(()).set(value)
        cond2 <- setter(None)
        result <- r(()).get
      } yield cond1 && !cond2 && result == Some(0)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - tryUpdate - modification occurs successfully") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryUpdate(_.map(_+ 1))
        value <- r(()).get
      } yield result && value == Some(1)

      op.map(a => assert(a === true))
    }

    // test("MapRef.ofSingleImmutableMapRef - tryUpdate - should fail to update if modification has occurred") {
    //   val updateRefUnsafely: Ref[IO, Option[Int]] => Unit = _.update(_.map(_ + 1)).unsafeRunSync()

    //   val op = for {
    //     r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
    //     _ <- r(()).set(Some(0))
    //     result <- r(()).tryUpdate(
    //       currentValue => {
    //         updateRefUnsafely(r(()))
    //         currentValue.map(_ + 1)
    //       }
    //     )
    //   } yield result

    //   op.map(a => assert(a === false))
    // }

    test("MapRef.ofSingleImmutableMapRef - tryModifyState - modification occurs successfully") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryModifyState(State.pure(Some(1)))
      } yield result.contains(Some(1))

      op.map(a => assert(a === true))
    }

    test("MapRef.ofSingleImmutableMapRef - modifyState - modification occurs successfully") {
      val op = for {
        r <- MapRef.ofSingleImmutableMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).modifyState(State.pure(Some(1)))
      } yield result == Some(1)

      op.map(a => assert(a === true))
    }


    test("MapRef.ofShardedImmutableMapRef - return an updated value") {
      val size = 10
      val key = 3
      val test = for {
        map <- MapRef.ofShardedImmutableMap[IO, Int, String](size)
        _ <- map(key).set("Foo".some)
        out <- map(key).get
      } yield out

      test.map(a => assert(a === Some("Foo")))
    }

    test("MapRef.ofShardedImmutableMapRef - work with convenience ops") {
        import io.chrisdavenport.mapref.implicits._
        val size = 10
        val key = 3
        val expect = "Foo"
        val test = for {
          map <- MapRef.ofShardedImmutableMap[IO, Int, String](size)
          _ <- map.setKeyValue(key, expect)
          out <- map(key).get
        } yield out

        test.map(a => assert(a === Some(expect)))
    }


    test("MapRef.ofConcurrentHashMap - concurrent modifications") {
      val finalValue = 100
      val r = MapRef.inConcurrentHashMap[SyncIO, IO, Unit, Int]().unsafeRunSync()
      val modifies = List.fill(finalValue)(r(()).update(_.map(_ + 1))).parSequence
      val test = r(()).set(Some(0)) *> modifies.start *> awaitEqual(r(()).get, finalValue.some)
      test.map(_ => assert(true))
    }

    test("MapRef.ofConcurrentHashMap - getAndSet - successful") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        getAndSetResult <- r(()).getAndSet(Some(1))
        getResult <- r(()).get
      } yield getAndSetResult == Some(0) && getResult == Some(1)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - access - successful") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield success && result == Some(1)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - access - setter should fail if value is modified before setter is called with None/Some") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        accessed <- r(()).access
        (value, setter) = accessed
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - access - setter should fail if value is modified before setter is called with init Some/Some") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        _ <- r(()).set(Some(5))
        success <- setter(value.map(_ + 1))
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - access - setter should fail if value is modified before setter is called with init Some/None") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        _ <- r(()).set(Some(5))
        success <- setter(None)
        result <- r(()).get
      } yield !success && result == Some(5)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - access - setter should fail if called twice") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        accessed <- r(()).access
        (value, setter) = accessed
        cond1 <- setter(value.map(_ + 1))
        _ <- r(()).set(value)
        cond2 <- setter(None)
        result <- r(()).get
      } yield cond1 && !cond2 && result == Some(0)

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - tryUpdate - modification occurs successfully") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryUpdate(_.map(_+ 1))
        value <- r(()).get
      } yield result && value == Some(1)

      op.map(a => assert(a === true))
    }

    // test("MapRef.ofConcurrentHashMap - tryUpdate - should fail to update if modification has occurred") {
    //   val updateRefUnsafely: Ref[IO, Option[Int]] => Unit = _.update(_.map(_ + 1)).unsafeRunSync()

    //   val op = for {
    //     r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
    //     _ <- r(()).set(Some(0))
    //     result <- r(()).tryUpdate(
    //       currentValue => {
    //         updateRefUnsafely(r(()))
    //         currentValue.map(_ + 1)
    //       }
    //     )
    //   } yield result

    //   op.map(a => assert(a === false))
    // }

    test("MapRef.ofConcurrentHashMap - tryModifyState - modification occurs successfully") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).tryModifyState(State.pure(Some(1)))
      } yield result.contains(Some(1))

      op.map(a => assert(a === true))
    }

    test("MapRef.ofConcurrentHashMap - modifyState - modification occurs successfully") {
      val op = for {
        r <- MapRef.ofConcurrentHashMap[IO, Unit, Int]()
        _ <- r(()).set(Some(0))
        result <- r(()).modifyState(State.pure(Some(1)))
      } yield result == Some(1)

      op.map(a => assert(a === true))
    }

  }
