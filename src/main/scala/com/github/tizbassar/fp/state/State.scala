package com.github.tizbassar.fp.state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if (count == 0) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        go(count - 1, r, i :: acc)
      }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = r => r.nextInt

  def unit[A](a: A): Rand[A] = r => (a, r)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    r => {
      val (a, r2) = s(r)
      (f(a), r2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r => {
      val (a, r2) = ra(r)
      val (b, r3) = rb(r2)
      (f(a, b), r3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(fs: List[Rand[A]], r: RNG, acc: List[A]): (List[A], RNG) = fs match {
      case Nil => (acc.reverse, r)
      case head :: tl => {
        val (a, r2) = head(r)
        go(tl, r2, a :: acc)
      }
    }
    r => go(fs, r, List())
  }

  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)((h, tl) => h :: tl))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    r => {
      val (a, r2) = f(r)
      g(a)(r2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2viaFlatMap[A, B, C](ar: Rand[A], br: Rand[B])(
    f: (A, B) => C
  ): Rand[C] =
    flatMap(ar) { a =>
      map(br) { b =>
        f(a, b)
      }
    }
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => {
      unit(f(a))
    })

  def map2[B, C](bs: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => bs.map(f(a, _)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((s, acc) => s.map2(acc)(_ :: _))

  def sequenceViaFoldLeft[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List()))(
      (acc, s) => s.map2(acc)((a, l) => a :: l)
    )

  def sequenceViaLoop[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def go(fs: List[State[S, A]], s: S, acc: List[A]): (List[A], S) = {
      fs match {
        case Nil => (acc.reverse, s)
        case head :: tl => {
          val (a, s2) = head.run(s)
          go(tl, s2, a :: acc)
        }
      }
    }
    State(s => go(fs, s, List()))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}
