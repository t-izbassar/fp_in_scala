package fp.monads
import fp.applicative.Applicative
import fp.testing._
import fp.testing.Prop._
import fp.parallelism._
import fp.parallelism.Par._
import fp.state.State
import fp.state.State._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      as map f
  }

  object Laws {
    // Identity law: map(x)(a => a) == x
    // This law guarantees that map preserve structure.
  }
}

/**
  * Monads provide a context for introducing and binding
  * variables and performing substitution.
  *
  * The chain of flatMap calls (for-comprehension) is like
  * an imperative program with statements assigned to
  * variables. The monad specifies what occurs at
  * statement boundaries.
  *
  * Monadic computations may choose structure dynamically,
  * based on the result of previous computations. It also
  * allows for context sensivity.
  *
  * Monad makes effects first class; they may be generated
  * at interpretation time, rather than chosen ahead of time.
  */
trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fab)((f: A => B) => map(fa)(f))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(Nil: List[A])) { (a: A, fla: F[List[A]]) =>
      flatMap(fla) { l: List[A] =>
        map(f(a))(p => if (p) a :: l else l)
      }
    }

  /**
    * Kleisli arrow: `A => F[B]`. The monad law can be expressed:
    *
    * {{
    * compose(compose(f, g), h) == compose(f, compose(g, h))
    * compose(f, unit) == f
    * compose(unit, f) == f
    * }}
    */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(a => f(a)))

  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(b => g(b)))
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ia: Id[A])(f: A => Id[B]): Id[B] = ia flatMap f
  }
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ga: Gen[A])(f: A => Gen[B]): Gen[B] =
      ga flatMap f
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(pa)(f)
  }

  def optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] =
      oa flatMap f
  }

  def listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
      la flatMap f
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }

  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    def flatMap[A, B](ea: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ea flatMap f
  }

  object Laws {

    // Associativity law: x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

    // Left identity law: flatMap(x)(unit) == x

    // Right identity law: flatMap(unit(y))(f) == f(y)

  }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(r => a)
    def flatMap[A, B](ra: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r =>
        val a = ra.run(r)
        f(a).run(r)
      }
  }
}
