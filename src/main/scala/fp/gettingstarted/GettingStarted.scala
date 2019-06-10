package fp.gettingstarted

object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, first: Int, second: Int): Int = {
      if (n <= 0) first
      else if (n == 1) second
      else go(n - 1, second, first + second)
    }
    go(n, 0, 1)
  }
}

object PolymorphicFunctions {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i + 1 == as.length) true
      else if (!gt(as(i), as(i + 1))) false
      else go(i + 1)
    }
    if (as.length <= 1) true
    else go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => partial1(a, f)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => {
      val g = f(a)
      g(b)
    }

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
