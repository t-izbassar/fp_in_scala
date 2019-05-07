package com.github.tizbassar.fp.parallelism

trait Par[A]

object Par {
  def run[A](a: Par[A]): A = ???

  def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def unit[A](a: A): Par[A] = ???
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
}
