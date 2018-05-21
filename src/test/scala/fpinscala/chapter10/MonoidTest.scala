package fpinscala.chapter10

import org.scalatest.FunSuite

/**
  * @author yamadanaoyuki
  */
class MonoidTest extends FunSuite {
  test("Ex 10.1 整数の加算と除算、および論理演算子に対するMonoidインスタンスを考えだせ。") {
    //
    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0
      override def op(a1: Int, a2: Int): Int = a1 + a2
    }
    assert(intAddition.op(2, 3) == 5)
    assert(intAddition.op(4, intAddition.zero) == 4)
    assert(
      intAddition.op(intAddition.op(2, 3), 4) == intAddition
        .op(2, intAddition.op(3, 4)))

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 1
      override def op(a1: Int, a2: Int): Int = a1 * a2
    }
    assert(intMultiplication.op(4, 5) == 20)
    assert(intMultiplication.op(intMultiplication.zero, 6) == 6)
    assert(
      intMultiplication.op(intMultiplication.op(2, 3), 4) == intMultiplication
        .op(2, intMultiplication.op(3, 4)))

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      // op(x, zero) == x になる、つまりxに何を入れても結果が変わらないもの、なのでorの場合はtrueになる。
      override def zero: Boolean = true
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    }
    assert(booleanOr.op(true, false) == true)
    assert(booleanOr.op(false, booleanOr.zero) == true)
    assert(booleanOr.op(false, true), false) == booleanOr.op(
      false,
      booleanOr.op(true, false))

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def zero: Boolean = false
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    }
    assert(booleanAnd.op(true, false) == false)
    assert(booleanAnd.op(true, true) == true)
    assert(booleanAnd.op(true, booleanAnd.zero) == false)
    assert(
      booleanAnd.op(booleanAnd.op(false, true), false) == booleanAnd
        .op(false, booleanAnd.op(true, false)))

  }

  test("Ex 10.2 Option型の値を結合するためのMonoidインスタンスを考えだせ。") {
    //
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def zero: Option[A] = None
      override def op(a1: Option[A], a2: Option[A]): Option[A] =
        (a1, a2) match {
          case (Some(x), Some(y)) => Some(x)
          case (Some(x), None)    => Some(x)
          case (None, Some(y))    => Some(y)
          case (None, None)       => None
        }
    }
    assert(optionMonoid.op(Some(2), Some(3)) == Some(2))
    assert(optionMonoid.op(Some(2), None) == Some(2))
    assert(optionMonoid.op(None, Some(3)) == Some(3))
    assert(optionMonoid.op(None, None) == None)

    //もっと簡単に書ける(本家の解答)
    def optionMonoid2[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(x: Option[A], y: Option[A]) = x orElse y
      val zero = None
    }
    assert(optionMonoid2.op(Some(2), Some(3)) == Some(2))
    assert(optionMonoid2.op(Some(2), None) == Some(2))
    assert(optionMonoid2.op(None, Some(3)) == Some(3))
    assert(optionMonoid2.op(None, None) == None)
  }

  test(
    "Ex 10.3 引数および戻り値の型が同じである関数をendo関数(endofunction)と呼ぶことがある。endo関数のモノイドを記述せよ。") {
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def zero: A => A = (a: A) => a
      override def op(a1: A => A, a2: A => A): A => A =
        a1 compose a2 //andThenでもよい
    }
    assert(
      endoMonoid[Int]
        .op((x: Int) => x + 1, (y: Int) => y * 2)(3) == (2 * 3) + 1)
    assert(endoMonoid[Int].op((x: Int) => x + 1, endoMonoid.zero)(2) == 3)
  }

  test(
    "Ex 10.4 PartII で作成したプロパティベースのテストフレームを使ってモノイド則のプロパティを実装せよ。これまでに記述したモノイドを、このプロパティを使ってテストせよ。") {
    //TODO
  }

  test("Ex 10.5 foldMapを実装せよ。") {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0
      override def op(a1: Int, a2: Int): Int = a1 + a2
    }

    //TODO どういうときに使うのか想像できない
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    assert(
      foldMap(List("abc", "d", "ef"), intAddition)((a: String) => a.length) == 6)
  }

  test(
    "Ex 10.6 難問：foldMap関数はfoldLeftまたはfoldRightを使って実装できる。ただし、foldMapを使ってfoldLeftとfoldRightを記述することも可能である。これを試せ。") {

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldMap
    //
  }

  test(
    "Ex 10.7 IndexedSeqのためのfoldMapを実装せよ。シーケンスを半分に分割し、両半分を再帰的に処理した後、モノイドを使ってそれらの結果を結合すること。") {
    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???
  }

  test(
    "Ex 10.8 難問：第7章で作成したライブラリを使って、並列化バージョンのfoldMapを実装せよ。ヒント：Monoid[A]をMonoid[Par[A]]に昇格させるコンビネータparを実装し、それを使ってparFoldMapを実装する。") {
    //TODO
  }

  test(
    "Ex 10.9 難問：foldMapを使って特定のIndexedSeq[Int]が順序付けされているかどうかを割り出せ。これについては、独創的なMonoidを考え出す必要があるだろう。") {
    //
  }

  test("Ex 10.10 WCのモノイドインスタンスを考え出し、そのインスタンスがモノイド則を満たすようにせよ。") {
    val wcMonoid: Monoid[WC] = ???
  }

  test(
    "Ex 10.11 WCモノイドを使ってStringに含まれている単語の数を数える関数を実装せよ。この関数は、文字列を再帰的に部分文字列に分解し、それらの部分文字列に含まれている単語を数える。") {
    //
  }

  test(
    "Ex 10.12 Foldable[List], Foldable[IndexedSeq], Foldable[Stream]を実装せよ。foldRight, foldLeft, foldMapは全てお互いをベースとして実装できるが、最も効率の良い実装ではないかもしれないことに注意。") {
    //
  }

  test("Ex 10.13 第3章で説明した二分木データ型TreeのFoldableインスタンスを実装せよ。") {
    //
    sealed trait Tree[A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  }

  test("Ex 10.14 Foldable[Option]インスタンスを記述せよ。") {
    //
  }

  test("Ex 10.15 Foldable構造は全てListに変換できる。この変換を汎用的な方法で記述せよ。") {
    //Monoid.scalaのほうに実装
  }

  test("Ex 10.16 この合成について証明せよ。A.opとB.opの両方が結合的である限り、opの実装が明らかに結合的であることに注意。") {
    //
    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = ???
  }

  test("Ex 10.17 結果がモノイドとなる関数のモノイドインスタンスを記述せよ。") {
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = ???
  }

  test(
    "Ex 10.18 bagは、要素ごとにエントリが1つ含まれたマップによって表される集合のようなものである。その要素はキーとして使用され、そのキーに基づいて格納された値はその要素がbagにいくつ含まれているかを示す。モノイドを使ってIndexedSeqからbagを計算せよ。") {
    /*
    scala > bag(Vector("a", "rose", "is", "a", "rose"))
    res0: Map[String, Int] = Map(a -> 2, rose -> 2, is -> 1)
     */
    def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???
  }

}
