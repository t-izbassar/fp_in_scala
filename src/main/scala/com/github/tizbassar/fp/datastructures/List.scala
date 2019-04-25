package com.github.tizbassar.fp.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (!f(x)) xs
      else dropWhile(xs)(f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}
