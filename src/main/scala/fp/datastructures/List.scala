package fp.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => throw new IllegalArgumentException("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => throw new IllegalArgumentException("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil
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
    case Nil          => throw new IllegalArgumentException("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def convert(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil                => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t)         => filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    ???
}
