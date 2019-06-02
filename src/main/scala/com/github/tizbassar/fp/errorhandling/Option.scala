package com.github.tizbassar.fp.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None      => None
    case Some(get) => Some(f(get))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None      => default
    case Some(get) => get
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    ao.flatMap(a => bo.map(b => f(a, b)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil        => Some(Nil)
    case head :: tl => map2(head, sequence(tl))(_ :: _)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil        => Some(Nil)
      case head :: tl => map2(f(head), traverse(tl)(f))(_ :: _)
    }

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)
}
