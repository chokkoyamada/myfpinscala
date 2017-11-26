package fpinscala.chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A

  //ex 10.15
  def toList[A](fa: F[A]): List[A] = ???
}
