package fpinscala.chapter03

import org.scalatest.FunSuite

class Test extends FunSuite {

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
    //Consでheadとtailに分割してtailを返せばよい
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
    // tailをn回繰り返すと考える
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
    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
      // tailをrecursionで適用する
      as match {
        case Nil => Nil
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case Cons(h, t) => Cons(h, t)
      }

    assert(dropWhile(List(1, 2, 3, 4, 5))(x => x < 3) == List(3, 4, 5))
  }

  test("Ex 3.6 Listの末尾を除くすべての要素で構成されたListを返すInit関数を実装せよ。この関数をtailのように一定時間で実装できないのはなぜか。") {
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => sys.error("init of Nil")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t)) //末尾再帰にはなっていない（できない）
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
        case Cons(h, t) => buf += h; go(t) //末尾再帰
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

  test("Ex 3.8 foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_._))のように、NilおよびCons自体をfoldRightに渡した場合はどうなるか。これがfoldRightとListのデータコンストラクタとの関係について何を表していると思うか。") {
    assert(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === Cons(1, Cons(2, Cons(3, Nil))))
    /*
    f(x, foldRight((xs, z)(f))
    Cons(x, foldRight(xs, z)(Cons(_,_))
    Cons(x, Cons(x, foldRight(xs, z)(Cons(_,_)))
    Cons(x, Cons(x, Cons(x, foldRight(xs, z)(Cons(_,_))))
    ...
    となる。
    foldRightはListのデータコンストラクタの実装と同じである。
     */
  }

  test("Ex 3.9 foldRightを使ってリストの長さを計算せよ。") {
    //素直に実装したバージョン
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

    //foldRightを使うバージョン
    def length2[A](as: List[A]): Int =
      List.foldRight(as, 0)((_, b) => b + 1)

    assert(length2(List(1, 2, 3, 4, 5)) == 5)
    assert(length2(List("a", "b", "c")) == 3)
    assert(length2(Nil) == 0)
  }

  test("Ex 3.10 前章で説明した手法を使って、リスト再帰の総称関数foldLeftを記述せよ。") {
    //foldLeftは左からたたみこむ、つまりConsのhead要素に対して関数を適用していく。
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    assert(foldLeft(List(3, 4, 5), 1)((a: Int, b: Int) => a + b) == 13)
    assert(foldLeft(List(3, 4, 5), 1)(_ + _) == 13) //カリー化されていて型推論できるためこのようにも書ける
    // foldLeft(List(4, 5), f(1, 3)((a: Int, b: Int) => a + b)
    // foldLeft(List(5), f(4, 4)((a: Int, b: Int) => a + b)
    // foldLeft(Nil, f(5, 8)((a: Int, b: Int) => a + b)
    // 13
  }

  test("Ex 3.11 foldLeftを使ってsum, product, およびリストの長さを計算する関数を記述せよ。") {
    def sum(ns: List[Int]): Int =
      List.foldLeft(ns, 0)(_ + _)

    assert(sum(List(1, 2, 3, 4)) == 10)

    def product(ns: List[Int]): Int =
      List.foldLeft(ns, 1)(_ * _)

    assert(product(List(1, 2, 3, 4)) == 24)

    def length(ns: List[Int]): Int =
      List.foldLeft(ns, 0)((z, _) => z + 1) //zを足し上げていくだけ

    assert(length(List(1, 2, 3, 4)) == 4)
  }

  test("Ex 3.12 要素が逆に並んだリストを返す関数を記述せよ。List(1, 2, 3)が与えられた場合、この関数はList(3, 2, 1)を返す。畳み込みを使って記述できるかどうかを確認すること。") {
    //素直に実装したバージョン
    def reverse[A](ns: List[A]): List[A] = {
      def go(ns: List[A], l: List[A]): List[A] =
        ns match {
          case Nil => l
          case Cons(h, t) => go(t, Cons(h, l))
        }

      go(ns, Nil)
    }
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(reverse(List(5, 3, 6, 9, 0)) == List(0, 9, 6, 3, 5))

    //foldLeftによる畳込みを使って記述したバージョン
    def reverse2[A](ns: List[A]): List[A] =
      List.foldLeft(ns, List[A]())((z, n) => Cons(n, z))

    assert(reverse2(List(1, 2, 3)) == List(3, 2, 1))
    assert(reverse2(List(5, 3, 6, 9, 0)) == List(0, 9, 6, 3, 5))
  }

  test("Ex 3.13 難問：foldRightをベースとして、foldLeftを記述することは可能か。その逆はどうか。foldLeftを使ってfoldRightを実装すると、foldRightを末尾再帰的に実装がすることが可能となり、大きなリストでもスタックオーバーフローが発生しなくなるので便利である。") {
    def foldLeftViaFoldRight[A, B](ns: List[A], z: B)(f: (A, B) => B): B =
      List.foldLeft(List.reverse(ns), z)((b, a) => f(a, b))

    assert(List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == foldLeftViaFoldRight(List(1, 2, 3, 4, 5), 0)(_ + _))

    //TODO 分からない。あとで見返す
    def foldLeftViaFoldRight2[A, B](ns: List[A], z: B)(f: (A, B) => B): B =
      List.foldLeft(ns, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    //TODO 分からない。あとで見返す
    def foldRightViaFoldLeft[A, B](ns: List[A], z: B)(f: (B, A) => B): B =
      List.foldRight(ns, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  }

  test("Ex 3.14 foldLeftまたはfoldRightをベースとしてappendを実装せよ。") {
    def append[A](l: List[A], r: List[A]): List[A] =
      List.foldRight(l, r)((a, b) => Cons(a, b)) //foldRightならできる。foldLeftはできない。

    append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)

  }

  test("Ex 3.15 難問：複数のリストからなるリストを1つのリストとして連結する関数を記述せよ。この関数の実行時間はすべてのリストの長さの合計に対して線形になるはずである。すでに定義した関数を使ってみること。") {
    def concat[A](l: List[List[A]]): List[A] =
      List.foldRight(l, List[A]())(List.appendViaFoldRight)

    assert(concat(List(List(1, 2), List(3, 4, 5))) == List(1, 2, 3, 4, 5))
  }

  test("Ex 3.16 各要素に1を足すことで整数のリストを変換する関数を記述せよ。注意：これは新しいListを返す純粋関数に鳴るはずである。") {
    def add(l: List[Int]): List[Int] = List.foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))
    //def add2(l: List[Int]): List[Int] = List.foldLeft(l, Nil:List[Int])((t, h) => Cons(h+1, t)) 逆になってしまうのでこれはできない

    assert(add(List(1, 2, 3)) == List(2, 3, 4))
  }

  test("Ex 3.17 List[Double]の各値をStringに変換する関数を記述せよ。d.toStringという式を使ってd: DoubleをStringに変換できる。") {
    def doubleToString(l: List[Double]): List[String] =
      List.foldRight(l, Nil:List[String])((a, b) => Cons(a.toString, b))

    assert(doubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

}
