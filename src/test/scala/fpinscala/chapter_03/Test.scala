package fpinscala.chapter_03

import org.scalatest.FunSuite

class Test extends FunSuite {

  import fpinscala.datastructures._

  test("Ex 3.1 次のマッチ式はどのような結果になるか。") {

    val sut = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //このパターンにマッチする
      case Cons(h, t)                            => h + List.sum(t)
      case _                                     => 102
    }

    assert(sut == 3)
  }

  test("Ex 3.2 Listの最初の要素を削除する関数tailを実装せよ。") {

    object ListNeo {
      def tail[A](l: List[A]): List[A] = l match {
        case Nil        => Nil
        case Cons(_, t) => t
      }
    }

    assert(ListNeo.tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  test("Ex 3.3 Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。") {

    object ListNeo {
      def setHead[A](l: List[A])(x: A): List[A] = l match {
        case Nil        => Nil
        case Cons(_, t) => Cons(x, t)
      }
    }

    assert(ListNeo.setHead(List(1, 2, 3, 4, 5))(0) === List(0, 2, 3, 4, 5))
  }

  test("Ex 3.4 リストの先頭からn個の要素を削除するdropという関数を実装せよ。") {
    def drop[A](as: List[A], n: Int): List[A] =
      if (n > 0) {
        as match {
          case Nil        => Nil
          case Cons(_, t) => drop(t, n - 1)
        }
      } else as

    assert(drop(List(1, 2, 3, 4, 5), 3) == List(4, 5))
    assert(drop(Nil, 1) == Nil)
  }
}
