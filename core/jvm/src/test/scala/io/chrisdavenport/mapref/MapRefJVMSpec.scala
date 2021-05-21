package io.chrisdavenport.mapref

import cats._
import cats.implicits._
import cats.data.State
import cats.effect.kernel.Ref
import cats.effect.{IO, SyncIO}

import scala.concurrent.duration._

class MapRefJVMSpec extends munit.CatsEffectSuite {
  private val smallDelay: IO[Unit] = IO.sleep(20.millis)
  private def awaitEqual[A: Eq](t: IO[A], success: A): IO[Unit] =
    t.flatMap(a => if (Eq[A].eqv(a, success)) IO.unit else smallDelay *> awaitEqual(t, success))

  test("MapRef.ofScalaConcurrentTrieMap - concurrent modifications") {
    val finalValue = 100
    val r = MapRef.inScalaConcurrentTrieMap[SyncIO, IO, Unit, Int].unsafeRunSync()
    val modifies = List.fill(finalValue)(r(()).update(_.map(_ + 1))).parSequence
    val test = r(()).set(Some(0)) *> modifies.start *> awaitEqual(r(()).get, finalValue.some)
    test.map(_ => assert(true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - getAndSet - successful") {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      getAndSetResult <- r(()).getAndSet(Some(1))
      getResult <- r(()).get
    } yield getAndSetResult == Some(0) && getResult == Some(1)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - access - successful" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      accessed <- r(()).access
      (value, setter) = accessed
      success <- setter(value.map(_ + 1))
      result <- r(()).get
    } yield success && result == Some(1)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - access - setter should fail if value is modified before setter is called with None/Some" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      accessed <- r(()).access
      (value, setter) = accessed
      _ <- r(()).set(Some(5))
      success <- setter(value.map(_ + 1))
      result <- r(()).get
    } yield !success && result == Some(5)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - access - setter should fail if value is modified before setter is called with init Some/Some" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      accessed <- r(()).access
      (value, setter) = accessed
      _ <- r(()).set(Some(5))
      success <- setter(value.map(_ + 1))
      result <- r(()).get
    } yield !success && result == Some(5)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - access - setter should fail if value is modified before setter is called with init Some/None" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      accessed <- r(()).access
      (value, setter) = accessed
      _ <- r(()).set(Some(5))
      success <- setter(None)
      result <- r(()).get
    } yield !success && result == Some(5)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - access - setter should fail if called twice" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
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

  test("MapRef.ofScalaConcurrentTrieMap - tryUpdate - modification occurs successfully" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      result <- r(()).tryUpdate(_.map(_+ 1))
      value <- r(()).get
    } yield result && value == Some(1)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - tryUpdate - should fail to update if modification has occurred" ) {
    val updateRefUnsafely: Ref[SyncIO, Option[Int]] => Unit = _.update(_.map(_ + 1)).unsafeRunSync()

    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[SyncIO, Unit, Int]
      _ <- r(()).set(Some(0))
      result <- r(()).tryUpdate(
        currentValue => {
          updateRefUnsafely(r(()))
          currentValue.map(_ + 1)
        }
      )
    } yield result

    op.map(a => assert(a === false))
  }

  test("MapRef.ofScalaConcurrentTrieMap - tryModifyState - modification occurs successfully" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      result <- r(()).tryModifyState(State.pure(Some(1)))
    } yield result.contains(Some(1))

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - modifyState - modification occurs successfully" ) {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      _ <- r(()).set(Some(0))
      result <- r(()).modifyState(State.pure(Some(1)))
    } yield result == Some(1)

    op.map(a => assert(a === true))
  }

  test("MapRef.ofScalaConcurrentTrieMap - Keys - empty") {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Unit, Int]
      result <- r.keys
    } yield result

    op.map(a => assert(a === Nil))
  }

  test("MapRef.ofScalaConcurrentTrieMap - keys - present") {
    val op = for {
      r <- MapRef.ofScalaConcurrentTrieMap[IO, Int, Int]
      _ <- r(1).set(Some(1))
      result <- r.keys
    } yield result

    op.map(a => assert(a === List(1)))
  }

}
