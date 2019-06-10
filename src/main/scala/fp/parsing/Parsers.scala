package fp.parsing
import fp.testing.Prop
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List()))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p) { a =>
      map(p2)(b => (a, b))
    }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a, b)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
    implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  val numA: Parser[Int] = char('a').many.map(_.size)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]) = self.product(p, p2)
    def **[B](p2: Parser[B]) = self.product(p, p2)
  }

  object Laws {
    def charLaw(c: Char) =
      run(char(c))(c.toString) == Right(c)

    def stringLaw(s: String) =
      run(string(s))(s) == Right(s)

    def orLaw =
      run(or(string("abra"), string("cadabra")))("abra") == Right("abra") &&
        run(or(string("abra"), string("cadabra")))("cadabra") == Right(
          "cadabra"
        )

    def suceedLaw[A](s: String, a: A) =
      run(succeed(a))(s) == Right(a)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Prop.Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Prop.Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
