package com.github.tizbassar.fp.testing
import com.github.tizbassar.fp.state.State
import com.github.tizbassar.fp.state.RNG
import com.github.tizbassar.fp.laziness.Stream
import Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop(
    (m, n, rng) =>
      run(m, n, rng) match {
        case Passed => p.run(m, n, rng)
        case x      => x
      }
  )

  def ||(p: Prop): Prop = Prop(
    (m, n, rng) =>
      run(m, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(m, n, rng)
        case x                 => x
      }
  )

  def tag(msg: String) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(failure, successes) =>
        Falsified(msg + "\n" + failure, successes)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SucessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SucessCount)
      extends Result {
    def isFalsified: Boolean = true
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) =>
      f(n, rng)
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(
            p =>
              Prop { (max, n, rng) =>
                p.run(max, casesPerSize, rng)
              }
          )
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  case class Gen[+A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))

    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))

    def unsized: SGen[A] = SGen(_ => this)
  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(
        State(
          RNG.map(RNG.nonNegativeInt)(n => n % (stopExclusive - start) + start)
        )
      )

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] =
      Gen(
        State(
          RNG.map(RNG.nonNegativeInt)(_ % 2 == 1)
        )
      )

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean flatMap (if (_) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen { n =>
        listOfN(n, g)
      }

  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = forSize(n)

    def map[B](f: A => B): SGen[B] =
      SGen { forSize(_) map f }

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen(n => {
        forSize(n) flatMap (f(_).forSize(n))
      })
  }
}
