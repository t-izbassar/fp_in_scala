package com.github.tizbassar.fp.laziness

import Stream._

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, tl) => f(h(), tl().foldRight(z)(f))
      case Empty       => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRigh(p: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) => if (p(h)) cons(h, t) else empty
    )

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) => if (p(h)) cons(h, t) else t
    )

  def append[AA >: A](s: Stream[AA]): Stream[AA] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, tl) => p(h()) || tl().exists(p)
    case Empty       => false
  }

  def toList: List[A] = this match {
    case Cons(h, tl) => h() :: tl().toList
    case Empty       => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, tl) if n == 1 => cons(h(), empty)
    case Cons(h, tl) if n > 1  => cons(h(), tl().take(n - 1))
    case _                     => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tl) if n > 0  => tl().drop(n - 1)
    case Cons(h, tl) if n == 0 => this
    case _                     => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, tl) if (p(h())) => cons(h(), tl().takeWhile(p))
    case _                       => empty
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, tl: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = tl
    Cons(() => head, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(first: => Int, second: => Int): Stream[Int] = {
      lazy val next = first + second
      cons(first, go(second, next))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])(x => cons(x._1, unfold(x._2)(f)))

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(x => Some((x._1, (x._2, x._1 + x._2))))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some((x, x)))

  def onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
