package fp.monoid
import fp.datastructures.Tree
import fp.datastructures.Leaf
import fp.datastructures.Branch
import fp.datastructures.Tree._

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(Nil: List[A])((a, l) => a :: l)
}

object Foldable {

  val foldableList: Foldable[List] = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero) { (a, b) =>
        mb.op(f(a), b)
      }
  }

  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }

    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a)      => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a)      => f(z, a)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
  }

  val foldableOption: Foldable[Option] = new Foldable[Option] {
    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None    => mb.zero
        case Some(a) => mb.op(f(a), mb.zero)
      }

    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as match {
        case None    => z
        case Some(a) => f(a, z)
      }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case None    => z
        case Some(a) => f(z, a)
      }
  }
}
