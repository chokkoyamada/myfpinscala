package fpinscala.chapter03

import org.scalatest.FunSuite

class ListTest extends FunSuite {

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
    //Consでheadとtailに分割してtailを返せばよい
    def tail[A](l: List[A]): List[A] = l match {
      case Nil        => Nil
      case Cons(_, t) => t
    }

    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  test("Ex 3.3 Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。") {

    def setHead[A](l: List[A])(x: A): List[A] = l match {
      case Nil        => Nil
      case Cons(_, t) => Cons(x, t)
    }

    assert(setHead(List(1, 2, 3, 4, 5))(0) === List(0, 2, 3, 4, 5))
  }

  test("Ex 3.4 リストの先頭からn個の要素を削除するdropという関数を実装せよ。") {
    // tailをn回繰り返すと考える
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

  test("Ex 3.5 述語とマッチする場合に限り、Listからその要素までを削除するdropWhileを実装せよ。") {
    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
      // tailをrecursionで適用する
      as match {
        case Nil                => Nil
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case Cons(h, t)         => Cons(h, t)
      }

    assert(dropWhile(List(1, 2, 3, 4, 5))(x => x < 3) == List(3, 4, 5))
  }

  test(
    "Ex 3.6 Listの末尾を除くすべての要素で構成されたListを返すInit関数を実装せよ。この関数をtailのように一定時間で実装できないのはなぜか。") {
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil          => sys.error("init of Nil")
        case Cons(_, Nil) => Nil
        case Cons(h, t)   => Cons(h, init(t)) //末尾再帰にはなっていない（できない）
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
        case Nil          => sys.error("init of Nil")
        case Cons(h, Nil) => List(buf.toList: _*)
        case Cons(h, t)   => buf += h; go(t) //末尾再帰
      }

      go(l)
    }

    assert(init2(List(1, 2, 3, 4)) == List(1, 2, 3))
  }

  test(
    "Ex 3.7 foldRightを使って実装されたproductは、0.0を検出した際に、直ちに再帰を中止して0.0を返せるか。その理由を説明せよ。") {
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

  test(
    "Ex 3.8 foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_._))のように、NilおよびCons自体をfoldRightに渡した場合はどうなるか。これがfoldRightとListのデータコンストラクタとの関係について何を表していると思うか。") {
    assert(
      List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === Cons(
        1,
        Cons(2, Cons(3, Nil))))
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
        case Nil        => n
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
      case Nil        => z
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

  test(
    "Ex 3.12 要素が逆に並んだリストを返す関数を記述せよ。List(1, 2, 3)が与えられた場合、この関数はList(3, 2, 1)を返す。畳み込みを使って記述できるかどうかを確認すること。") {
    //素直に実装したバージョン
    def reverse[A](ns: List[A]): List[A] = {
      def go(ns: List[A], l: List[A]): List[A] =
        ns match {
          case Nil        => l
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

  test(
    "Ex 3.13 難問：foldRightをベースとして、foldLeftを記述することは可能か。その逆はどうか。foldLeftを使ってfoldRightを実装すると、foldRightを末尾再帰的に実装がすることが可能となり、大きなリストでもスタックオーバーフローが発生しなくなるので便利である。") {
    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      List.foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    assert(
      List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == foldLeftViaFoldRight(
        List(1, 2, 3, 4, 5),
        0)(_ + _))

    //TODO 分からない。あとで見返す
    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      List.foldLeft(List.reverse(l), z)((b, a) => f(a, b))

    //TODO 分からない。あとで見返す
    def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      List.foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  test("Ex 3.14 foldLeftまたはfoldRightをベースとしてappendを実装せよ。") {
    def append[A](l: List[A], r: List[A]): List[A] =
      List.foldRight(l, r)((a, b) => Cons(a, b)) //foldRightならできる。foldLeftはできない。

    append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)

  }

  test(
    "Ex 3.15 難問：複数のリストからなるリストを1つのリストとして連結する関数を記述せよ。この関数の実行時間はすべてのリストの長さの合計に対して線形になるはずである。すでに定義した関数を使ってみること。") {
    def concat[A](l: List[List[A]]): List[A] =
      List.foldRight(l, List[A]())(List.appendViaFoldRight)

    assert(concat(List(List(1, 2), List(3, 4, 5))) == List(1, 2, 3, 4, 5))
  }

  test("Ex 3.16 各要素に1を足すことで整数のリストを変換する関数を記述せよ。注意：これは新しいListを返す純粋関数に鳴るはずである。") {
    def add(l: List[Int]): List[Int] =
      List.foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
    //def add2(l: List[Int]): List[Int] = List.foldLeft(l, Nil:List[Int])((t, h) => Cons(h+1, t)) 逆になってしまうのでこれはできない

    assert(add(List(1, 2, 3)) == List(2, 3, 4))
  }

  test(
    "Ex 3.17 List[Double]の各値をStringに変換する関数を記述せよ。d.toStringという式を使ってd: DoubleをStringに変換できる。") {
    def doubleToString(l: List[Double]): List[String] =
      List.foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

    assert(doubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

  test("Ex 3.18 リストの各要素を変更し、かつリストの構造をそのまま保つ総称関数mapを記述せよ。この関数のシグネチャは以下の通り。") {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      List.foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    assert(map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
    assert(map(List(5, 2, 4))(_.toString) == List("5", "2", "4"))

    //スタックオーバーフローを避けるにはfoldLeftを使ったバージョンにすべき
    def map_1[A, B](l: List[A])(f: A => B): List[B] =
      List.foldRightViaFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))

    //関数内でmutableない状態を使っても参照透過性は損なわれないため、問題ない
    def map_2[A, B](l: List[A])(f: A => B): List[B] = {
      val buf = new collection.mutable.ListBuffer[B]
      def go(l: List[A]): Unit = l match {
        case Nil        => ()
        case Cons(h, t) => buf += f(h); go(t)
      }
      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }
  }

  test(
    "Ex 3.19 与えられた述語要件が満たされるまでリストから要素を削除するfilter関数を記述せよ。この関数を使ってList[Int]から奇数を全て削除せよ。") {
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      List.foldRight(as, Nil: List[A])((h, z) => if (f(h)) Cons(h, z) else z)

    assert(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))
    assert(filter(List(24, 1, 43, 39, 9))(_ % 2 == 0) == List(24))

    //mapと同じでスタックオーバーフローを避けるバージョン
    def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
      List.foldRightViaFoldLeft(l, Nil: List[A])((h, t) =>
        if (f(h)) Cons(h, t) else t)

    //mapと同じで、関数内部でmutableを使うのは問題ない
    def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
      val buf = new collection.mutable.ListBuffer[A]
      def go(l: List[A]): Unit = l match {
        case Nil        => ()
        case Cons(h, t) => if (f(h)) buf += h; go(t)
      }
      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }
  }

  test(
    "Ex 3.20 mapと同じような働きをするflatMap関数を記述せよ。この関数は単一の結果ではなくリストを返し、そのリストは最終的な結果のリストに挿入されなければならない。この関数のシグネチャは以下の通り。例えばflatMap(List(1, 2, 3))(i => List(i, i))はList(1, 1, 2, 2, 3, 3)になるはずである。") {
    //concatはflatten? mapしたものを平坦にするイメージ
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      List.concat(List.map(as)(f))

    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  test("Ex 3.21 flatMapを使ってfilterを実装せよ。") {
    def filter[A, B](as: List[A])(f: A => Boolean): List[A] =
      List.flatMap(as)(a => if (f(a)) List(a) else Nil)

    assert(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))
  }

  test(
    "Ex 3.22 リストを2つ受取り、対応する要素同士を足し合わせて新しいリストを生成する関数を記述せよ。たとえばList(1, 2, 3)とList(4, 5, 6)はList(5, 7, 9)になる。") {
    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

    assert(addPairwise(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  test(
    "Ex 3.23 EXERCISE 3.22で作成した関数を、整数または加算に限定されないように一般化せよ。一般化された関数にはzipWithという名前をつけること。") {
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
      (a, b) match {
        case (Nil, _)                     => Nil
        case (_, Nil)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }

    assert(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
  }

  test(
    "Ex 3.24 難問：例として、Listに別のListがサブシーケンスとして含まれているかどうかを調べるhasSubsequenceを実装せよ。たとえばList(1, 2, 3, 4)にはList(1, 2), List(2, 3), List(4)などがサブシーケンスとして含まれている。純粋関数型で、コンパクトで、かつ効率的な実装を見つけ出すのは難しいかもしれない。その場合は、それで構わない。どのようなものであれ、最も自然な関数を実装すること。この実装については、第5章であらためて取り上げ、改良する予定である。なおScalaでは、任意の値xおよびyに対し、x == yという式を使って等しいかどうかを比較できる。") {
    //自分で実装バージョン
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
      sup match {
        case Nil => false
        case _ =>
          sub match {
            case Nil => true
            case Cons(h, t) =>
              hasSubsequence(List.dropWhile(sup)(a => a != h), t)
          }
      }

    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(3, 2)))

    //公式解答バージョン
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean =
      (l, prefix) match {
        case (_, Nil)                              => true
        case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
        case _                                     => false
      }
    @annotation.tailrec
    def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h, t)                => hasSubsequence2(t, sub)
    }
    assert(hasSubsequence2(List(1, 2, 3, 4), List(1, 2)))
    assert(hasSubsequence2(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence2(List(1, 2, 3, 4), List(4)))
    assert(!hasSubsequence2(List(1, 2, 3, 4), List(3, 2)))
  }

}
