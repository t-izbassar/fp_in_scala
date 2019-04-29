package com.github.tizbassar.fp.laziness

import Stream._

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, tl) => f(h(), tl().foldRight(z)(f))
      case Empty       => z
    }

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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
