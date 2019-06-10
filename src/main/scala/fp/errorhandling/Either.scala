package fp.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value)  => Left(value)
    case Right(value) => Right(f(value))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value)  => b
      case Right(value) => this
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(b => f(a, b)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil        => Right(Nil)
      case head :: tl => head.map2(sequence(tl))(_ :: _)
    }

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]
  ): Either[E, List[B]] = as match {
    case Nil        => Right(Nil)
    case head :: tl => f(head).map2(traverse(tl)(f))(_ :: _)
  }
}
