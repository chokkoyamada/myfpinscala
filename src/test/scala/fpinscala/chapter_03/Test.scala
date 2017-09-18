package fpinscala.chapter_03

import org.scalatest.FunSuite

class Test extends FunSuite {

  import fpinscala.datastructures._

  test("Ex 3.1 次のマッチ式はどのような結果になるか。") {

    val sut = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //このパターンにマッチする
      case Cons(h, t) => h + List.sum(t)
      case _ => 102
    }

    assert(sut == 3)
  }

  test("Ex 3.2 Listの最初の要素を削除する関数tailを実装せよ。") {

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))

  }

  test("Ex 3.3 Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。") {

    def setHead[A](l: List[A])(x: A): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => Cons(x, t)
    }

    assert(setHead(List(1, 2, 3, 4, 5))(0) === List(0, 2, 3, 4, 5))
  }

  test("Ex 3.4 リストの先頭からn個の要素を削除するdropという関数を実装せよ。") {
    def drop[A](as: List[A], n: Int): List[A] =
      if (n > 0) {
        as match {
          case Nil => Nil
          case Cons(_, t) => drop(t, n - 1)
        }
      } else as

    assert(drop(List(1, 2, 3, 4, 5), 3) == List(4, 5))
    assert(drop(Nil, 1) == Nil)
  }

  test("Ex 3.5 述語とマッチする場合に限り、Listからその要素までを削除するdropWhileを実装せよ。") {
    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case Cons(h, t) => Cons(h, t)
      }
    }

    assert(dropWhile(List(1, 2, 3, 4, 5))(x => x < 3) == List(3, 4, 5))
  }

  test("Ex 3.6 Listの末尾を除くすべての要素で構成されたListを返すInit関数を実装せよ。この関数をtailのように一定時間で実装できないのはなぜか。") {
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => sys.error("init of Nil")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }

    /*
    List(1, 2, 3, 4)のときのトレース

    Cons(1, init(List(2, 3, 4)))
    Cons(1, Cons(2, init(List(3, 4)))))
    Cons(1, Cons(2, Cons(3, init(List(4, Nil))))))
    Cons(1, Cons(2, Cons(3, Nil))))
    List(1, 2, 3)

    このように、スタックに一度つみあげる必要があるため、一定時間では実装できない。

     */

    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))

    //ListBufferを使うバージョン。mutableを使うが、関数内に閉じているので、参照透過性を破っていない。
    def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of Nil")
        case Cons(h, Nil) => List(buf.toList: _*)
        case Cons(h, t) => buf += h; go(t)
      }

      go(l)
    }

    assert(init2(List(1, 2, 3, 4)) == List(1, 2, 3))
  }

  test("Ex 3.7 foldRightを使って実装されたproductは、0.0を検出した際に、直ちに再帰を中止して0.0を返せるか。その理由を説明せよ。") {
    assert(List.product2(List(1.0, 2.0, 3.0, 0.0, 5.0)) == 0.0)
    /*
    再帰を途中で中止することはできない。理由はfoldRightはリストを最後まで走査してからでなければ畳み込みを開始できないから。

    product2( List(1.0, 2.0, 3.0, 0.0, 5.0) )
    foldRight( List(1.0, 2.0, 3.0, 0.0, 5.0), 1.0 )(f)

    f(
      1.0, foldRight(List(2.0, 3.0, 0.0, 5.0), 1.0)(f)
    )(f)

    f(
      1.0,
        f(
          2.0, foldRight(List(3.0, 0.0, 5.0), 1.0)(f)
        )(f)
    )(f)

    f(
      1.0,
        f(
          2.0,
            f(
              3.0, foldRight(List(0.0, 5.0), 1.0)(f)
            )(f)
        )(f)
    )(f)

    f(
      1.0,
        f(
          2.0,
            f(
              3.0,
                f(
                  0.0, foldRight(List(5.0), 1.0)(f)
                )(f)
            )(f)
        )(f)
    )(f)

    f(
      1.0,
        f(
          2.0,
            f(
              3.0,
                f(
                  0.0,
                    f(
                      5.0, foldRight(Nil, 1.0)(f)
                    )(f)
                )(f)
            )(f)
        )(f)
    )(f)


    f(
      1.0,
        f(
          2.0,
            f(
              3.0,
                f(
                  0.0,
                    f(
                      5.0,
                        1.0
                    )(f)
                )(f)
            )(f)
        )(f)
    )(f)
     */
  }

  test("Ex. 3.8 foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_._))のように、NilおよびCons自体をfoldRightに渡した場合はどうなるか。これがfoldRightとListのデータコンストラクタとの関係について何を表していると思うか。") {
    assert(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === Cons(1, Cons(2, Cons(3, Nil))))
    /*
    f(x, foldRight((xs, z)(f))
    Cons(x, foldRight(xs, z)(Cons(_,_))
    Cons(x, Cons(x, foldRight(xs, z)(Cons(_,_)))
    Cons(x, Cons(x, Cons(x, foldRight(xs, z)(Cons(_,_))))
    ...
    となる。
     */
  }

  test("Ex 3.9 foldRightを使ってリストの長さを計算せよ。") {
    def length[A](as: List[A]): Int = {
      def go(n: Int, l: List[A]): Int = l match {
        case Nil => n
        case Cons(h, t) => go(n + 1, t)
      }

      go(0, as)
    }

    assert(length(List(1, 2, 3, 4, 5)) == 5)
    assert(length(List("a", "b", "c")) == 3)
    assert(length(Nil) == 0)
  }

  test("Ex 3.10 前章で説明した手法を使って、リスト再帰の総称関数foldLeftを記述せよ。") {
    //foldLeftは左からたたみこむ、つまりConsのhead要素に対して関数を適用していく。
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }
}
