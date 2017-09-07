package fpinscala

import fpinscala.Stream._

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {
  //5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toList_2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  //5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }




  //5.5
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])(
      (h, t) =>
        if (p(h)) cons(h, t)
        else empty)
  }

  //5.6
  def headOptionWithFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  // 5.7
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }
  /*
  def flatMap[B](f: A => Option[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h)
  }
  */
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //5.8
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def constant_2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }


  //5.9
  def from(n: Int): Stream[Int] = ???


  //5.10
  def fibs(n: Int): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f0 + f1, f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def main(args: Array[String]) = {

  }
}

