package fpinscala.chapter03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  //foldRightは右から畳み込む、つまりconsのtail
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, List[A]())((z, n) => Cons(n, z))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, b) => Cons(a, b)) //foldRightならできる。foldLeftはできない。

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def concat[A](l: List[List[A]]): List[A] =
    List.foldRight(l, List[A]())(List.appendViaFoldRight)

  def map[A, B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    List.concat(List.map(as)(f))

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil                => Nil
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case Cons(h, t)         => Cons(h, t)
    }
}
