package com.github.tizbassar.fp.testing
import com.github.tizbassar.fp.state.State
import com.github.tizbassar.fp.state.RNG

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = Prop.this.check && p.check
  }
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
