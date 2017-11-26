package fpinscala.chapter05

import Stream._

trait Stream[+A] {
  //末尾再帰ではないのでスタックオーバーフローになる可能性がある
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _          => List()
  }

  //末尾再帰バージョン
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }
    go(this, List()).reverse
  }

  //ListBufferを使ってメソッド内でmutableを使うバージョン
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buf += h(); go(t())
      case _          => buf.toList
    }
    go(this)
  }

  //ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  //ex 5.2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match { //tailrecの場合はprivateかfinalでなければならない
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

  //ex 5.3
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  //ex 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //5.5
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])(
      (h, t) =>
        if (p(h)) cons(h, t)
        else empty)
  }

  //5.6 TODO 難問
  def headOptionWithFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  //5.7
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  //5.7
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (f(h)) cons(h, t)
        else t)

  //5.7
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  //TODO 分からない。頭が働いているときに見直す
  //5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

  //5.11 TODO
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => Stream.cons(h, unfold(s)(f))
    case None         => Stream.empty
  }

  //5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _          => None
    }

  //5.13
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _                        => None
    }

  //5.13
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _                    => None
    }

  //5.13
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  //5.13
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  //5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  //5.13
  def zipWithAll[B, C](s2: Stream[B])(
      f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Empty) =>
        Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) =>
        Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      case (Empty, Empty) => None
      case _              => None
      /* TODO case _ を入れないと下記のwarningがでる
      [warn] /Users/yamadanaoyuki/Documents/git/myfpinscala/src/main/scala/fpinscala/chapter05/Stream.scala:154: match may not be exhaustive.
      arn] It would fail on the following inputs: (Cons(_, _), _), (Empty, _), (Stream(), _), (_, Cons(_, _)), (_, Empty), (_, Stream()), (_, _)
      [warn]     unfold((this, s2)) {
     */
    }

  //5.14 TODO もはや自分で実装できる気がしないが...
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhileWithFoldRight(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  //5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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

  //5.8
  def constantInefficient[A](a: A): Stream[A] = {
    cons(a, constantInefficient(a))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  //5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //5.10
  def fibs(n: Int): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty
    }

}
