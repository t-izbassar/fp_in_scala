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

object Applicative {

  /*
  Applicative must obey functor laws:
  map(v)(id) == v
  map(map(v)(g))(f) == map(v)(f compose g)

  Left identity:  map2(unit(()), fa)((_, a) => a) == fa
  Right identity: map2(fa, unit(()))((a, _) => a) == fa

  Associativity: product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(assoc)
  where assoc is:
  {{
  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = p match {
    case (a, (b, c)) => ((a, b), c)
  }
  }}

  Naturality law: map2(a, b)(prodcutF(f, g)) == product(map(a)(f), map(b)(g))
  where productF:
  {{
  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))
  }}
 */
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  // def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f]
  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(
        f: (A, B) => C
      ): Validation[E, C] =
        (va, vb) match {
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e @ Failure(_, _), Success(_)) => e
          case (Success(_), e @ Failure(_, _)) => e
          case (Success(a), Success(b))        => Success(f(a, b))
        }
    }
}
