package io.chrisdavenport.mapref

import cats._
import cats.implicits._
import cats.effect.concurrent.Ref

trait RefLite[F[_], A]{
  def get: F[A]
  def set(a: A): F[Unit]
  def getAndSet(a: A): F[A]
  def tryUpdate(f: A => A): F[Boolean]
  def tryModify[B](f: A => (A, B)): F[Option[B]]
  def update(f: A => A): F[Unit]
  def modify[B](f: A => (A, B)): F[B]

  def mapK[G[_]](fk: F ~> G): RefLite[G, A] =
    new RefLite.TransformedRef[F, G, A](this, fk)
}

object RefLite {

  implicit def fromRef[F[_], A](ref: Ref[F, A]): RefLite[F, A] = 
    new RefLite[F, A] {
      def get: F[A] = ref.get
      def set(a: A): F[Unit] = ref.set(a)
      def getAndSet(a: A): F[A] = ref.getAndSet(a)
      def tryUpdate(f: A => A): F[Boolean] = ref.tryUpdate(f)
      def tryModify[B](f: A => (A, B)): F[Option[B]] = 
        ref.tryModify(f)
      def update(f: A => A): F[Unit] = ref.update(f)
      def modify[B](f: A => (A, B)): F[B] = ref.modify(f)
    }

  implicit def catsInvariantForRefLite[F[_]: Functor]: Invariant[RefLite[F, ?]] =
    new Invariant[RefLite[F, ?]] {
      override def imap[A, B](fa: RefLite[F, A])(f: A => B)(g: B => A): RefLite[F, B] =
        new RefLite[F, B] {
          override val get: F[B] = fa.get.map(f)
          override def set(a: B): F[Unit] = fa.set(g(a))
          override def getAndSet(a: B): F[B] = fa.getAndSet(g(a)).map(f)
          override def tryUpdate(f2: B => B): F[Boolean] =
            fa.tryUpdate(g compose f2 compose f)
          override def tryModify[C](f2: B => (B, C)): F[Option[C]] =
            fa.tryModify (
              f2.compose(f).map(_.leftMap(g))
            )
          override def update(f2: B => B): F[Unit] =
            fa.update(g compose f2 compose f)
          override def modify[C](f2: B => (B, C)): F[C] = fa.modify (
            f2.compose(f).map(_.leftMap(g))
          )
        }
    }

  private final class TransformedRef[F[_], G[_], A](
    underlying: RefLite[F, A],
    trans: F ~> G
  ) extends RefLite[G, A]{
    override def get: G[A] = trans(underlying.get)
    override def set(a: A): G[Unit] = trans(underlying.set(a))
    override def getAndSet(a: A): G[A] = trans(underlying.getAndSet(a))
    override def tryUpdate(f: A => A): G[Boolean] = trans(underlying.tryUpdate(f))
    override def tryModify[B](f: A => (A, B)): G[Option[B]] = trans(underlying.tryModify(f))
    override def update(f: A => A): G[Unit] = trans(underlying.update(f))
    override def modify[B](f: A => (A, B)): G[B] = trans(underlying.modify(f))
  }


}