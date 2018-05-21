package fpinscala.chapter12

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
trait Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))
  def unit[A](a: => A): F[A] = ???

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

/*
object Hoge {
  def validationApplicative[E]
    : Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      override def apply[A, B](fab: Validation[E, A => B])(
          fa: Validation[E, A]): Validation[E, B] =
        map2(fab, fa)((ab, a) => ab(a))
      override def unit[A](a: => A): Validation[E, A] = Success(a)


      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    }
}
 */
trait Monad[F[_]]extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B] (ma: F[A])(f:A => F[B]): F[B]
  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}


trait Traverse[F[_]] extends Functor[F] {
  type Id[A] = A

  /*
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }
   */

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
    implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[G[_], A, B](fa: F[A])(f: A => B)(implicit G: Applicative[G]): F[B] = ???

  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[(F[G[x$$]]) forSome {type x$$}] {
      override def flatMap[A, B](ma: (F[G[x$$]]) forSome {type x$$} = ???
      override def unit[A](a: => A): (F[G[x$$]]) forSome {type x$$} = ???
    }

}