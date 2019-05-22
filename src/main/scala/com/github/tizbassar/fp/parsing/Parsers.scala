package com.github.tizbassar.fp.parsing
import com.github.tizbassar.fp.testing.Prop

trait Parsers[ParseError, Parser[+ _]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  val numA: Parser[Int] = char('a').many.map(_.size)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
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
