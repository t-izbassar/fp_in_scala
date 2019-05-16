package com.github.tizbassar.fp.testing
import com.github.tizbassar.fp.state.State
import com.github.tizbassar.fp.state.RNG
import com.github.tizbassar.fp.laziness.Stream
import Prop._

object Prop {
  type FailedCase = String
  type SucessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop(
    (n, rng) =>
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case x      => x
      }
  )

  def ||(p: Prop): Prop = Prop(
    (n, rng) =>
      run(n, rng) match {
        case Falsified(_, _) => p.run(n, rng)
        case x               => x
      }
  )
}

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

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))
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
}
