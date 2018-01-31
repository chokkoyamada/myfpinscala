package fpinscala.monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2
    override def zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]) = a1 ++ a2
    override def zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2
    override def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2
    override def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2
    override def zero = true
  }
}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A

  def toList[A](as: F[A]): List[A]
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  override def concatenate[A](as: List[A])(m: Monoid[A]): A = ???
  override def toList[A](as: List[A]): List[A] = as
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Some(x) => f(x, z)
      case _       => f(None, z)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Some(x) => f(z, x)
      case _       => f(z, None)
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(x) => f(x)
    }

  override def toList[A](as: Option[A]) = as match {
    case Some(x) => List(x)
    case _       => Nil
  }

  override def concatenate[A](as: Option[A])(m: Monoid[A]) = ???

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B) = a => B.op(a1(a), a2(a))
      override def zero: A => B = _ => B.zero
    }
  }
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M: Monoid[Map[A, Int]] = mapMergeMonoid(new Monoid[Int]{
      override def op(a1: Int, a2: Int) = a1 + a2
      override def zero = 0
    })
    val p = ListFoldable.foldMap(as.toList)(a => a -> 1 )(M)

  }
}
