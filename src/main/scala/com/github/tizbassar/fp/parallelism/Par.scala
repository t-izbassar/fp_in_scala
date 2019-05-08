package com.github.tizbassar.fp.parallelism
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def unit[A](a: A): Par[A] = es => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequenceViaFoldRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit[List[A]](List()))((pa, acc) => map2(pa, acc)(_ :: _))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map {
      asyncF { a =>
        if (f(a)) List(a) else List()
      }
    }
    map(sequence(pars))(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled(): Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](
      a: Future[A],
      b: Future[B],
      f: (A, B) => C
  ) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone(): Boolean = cache.isDefined
    def isCancelled(): Boolean = a.isCancelled() || b.isCancelled()
    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get(): C = compute(Long.MaxValue)
    def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val av = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val aTime = System.nanoTime - start
        val bv = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(av, bv)
        cache = Some(ret)
        ret
    }
  }
}
