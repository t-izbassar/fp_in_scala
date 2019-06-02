package com.github.tizbassar.fp.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value)         => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value)         => value
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value)         => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)(1 + _ + _)

  def maxViaFold(tree: Tree[Int]): Int =
    fold(tree)(x => x)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 0)((x, y) => 1 + (x max y))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}
