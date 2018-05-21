package fpinscala.chapter13

import fpinscala.chapter12.Monad

object IO {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  // Exercise 1: Implement the free monad
  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f] {
      def unit[A](a: => A): Free[F, A] = Return(a)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
    }

  // Exercise 2: Implement a specialized `Function0` interpreter.
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(x)  => x
    case Suspend(x) => x()
    case FlatMap(x, f: () => A) =>
      x match {
        case Return(y:A) =>
          runTrampoline {
            f(y)
          }
        case Suspend(y:A) =>
          runTrampoline {
            f(y())
          }
        case FlatMap(y:A, g: () => A) =>
          runTrampoline {
            y flatMap { z =>
              g(z).flatMap(f)
            }
          }
      }
  }

  // Exercise 3: Implement a `Free` interpreter which works for any `Monad`
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a)              => F.unit(a)
    case Suspend(r)             => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _                      => sys.error("Impossible, since `step` eliminates these cases")
  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => a
  }
}
