package com.github.tizbassar.fp.monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v.head)
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = List()
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean =
      a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean =
      a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A =
      a1 andThen a2
    override def zero: A => A = identity
  }

  import com.github.tizbassar.fp.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)
    ) { case (a, b, c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) } && forAll(
      gen
    )(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)
}
