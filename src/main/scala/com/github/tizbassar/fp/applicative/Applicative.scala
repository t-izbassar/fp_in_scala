package com.github.tizbassar.fp.applicative
import com.github.tizbassar.fp.monads.Functor

/**
  * With Applicative the structure of computation
  * is fixed. Applicative sequence effects. It also
  * constructs context-free computations.
  */
trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa) { (f: A => B, a: A) =>
      f(a)
    }

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(mapViaApply(fa)(f.curried): F[B => C])(fb)

  // f.curried: A => B => C => D
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
    f: (A, B, C) => D
  ): F[D] = {
    def g(a: A, b: B): C => D = f.curried(a)(b)
    apply(map2ViaApply(fa, fb)(g))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
    f: (A, B, C, D) => E
  ): F[E] = {
    def g(a: A, b: B, c: C): D => E = f.curried(a)(b)(c)
    apply(map3(fa, fb, fc)(g))(fd)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(Nil: List[B])) { (a, fbs) =>
      map2(f(a), fbs)(_ :: _)
    }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))
}
