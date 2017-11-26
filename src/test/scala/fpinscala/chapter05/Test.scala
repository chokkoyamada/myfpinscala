package fpinscala.chapter05

import org.scalatest.FunSuite

class Test extends FunSuite {
  test(
    "Ex 5.1 StreamをListに変換し、それによりストリームを強制的に評価する関数を記述せよ。結果はREPLで確認できる。標準ライブラリの通樹のList型への変換が可能である。この関数と、Streamを操作する他の関数は、Stream Traitの中に配置できる。") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test(
    "Ex 5.2 Streamの先頭からn個の要素を取り出す関数take(n)と、Streamの先頭からn個の要素をスキップするdrop(n)関数ぞ記述せよ。") {
    assert(Stream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))
  }

  test("Ex 5.3 Streamの先頭から指定された述語とマッチする要素を先頭から取り出すtakeWhile関数を記述せよ。") {
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList == List(1, 2))
  }

  test(
    "Ex 5.4 Streamの要素のうち、指定された述語とマッチするものを全てチェックするforAllを実装せよ。この実装では、マッチしない値が検出された時点でチェックを終了しなければならない。") {
    assert(Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ > 10))
  }

  test("Ex 5.5 foldRightを使ってtakeWhileを実装せよ。") {
    assert(
      Stream(1, 2, 3, 4, 5).takeWhileWithFoldRight(_ < 3).toList == List(1, 2))
  }

  test("Ex 5.6 foldRightを使ってheadOptionを実装せよ。") {
    assert(Stream(1, 2, 3, 4, 5).headOptionWithFoldRight.contains(1))
    assert(Stream().headOptionWithFoldRight.isEmpty)
  }

  test(
    "Ex 5.7 foldRightを使ってmap, filter, append, flatMapを実装せよ。appendメソッドはその引数に対して非正格でなければならない。") {
    assert(Stream(1, 2, 3, 4, 5).map(_ * 2).toList == List(2, 4, 6, 8, 10))

    assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))

    assert(Stream(1, 2).append(Stream(6, 7, 8)).toList == List(1, 2, 6, 7, 8))

    assert(
      Stream(1, 2, 3, 4, 5)
        .flatMap(x => Stream(x * 2))
        .toList == List(2, 4, 6, 8, 10))
  }

  test("Ex 5.8 onesを少し一般化し、指定された値の無限ストリームを返すconstant関数を実装せよ。") {
    def ones: Stream[Int] = Stream.cons(1, ones)

    assert(ones.take(3).toList == List(1, 1, 1))

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    assert(constant(3).take(5).toList == List(3, 3, 3, 3, 3))
  }

  test("Ex 5.9 nで始まってn + 1, n + 2と続く整数の無限ストリームを生成する関数を記述せよ。") {
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    assert(from(1).take(5).toList == List(1, 2, 3, 4, 5))
  }

  test("Ex 5.10 フィボナッチ数列(0, 1, 1, 2, 3, 5, 8...)の無限ストリームを生成するfibs関数を記述せよ。") {
    def fibs(): Stream[Int] = {
      // フィボナッチ数列を作るためには、前2つの値を引数とした関数が必要。それを作り、その再帰関数をStreamにする。
      def go(f0: Int, f1: Int): Stream[Int] =
        Stream.cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }

    assert(fibs().take(5).toList == List(0, 1, 1, 2, 3))
  }

  test(
    "Ex 5.11 より汎用的なストリーム生成関数unfoldを記述せよ。この関数は、初期状態に加えて、以下の状態と、生成されるストリームの次の値を生成する関数を受け取る。") {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, s)) => Stream.cons(h, unfold(s)(f))
      case None         => Stream.empty
    }

    //fromと同じ
    assert(
      unfold(3)(n => Some((n, n + 1))).take(5).toList == List(3, 4, 5, 6, 7))

    import Stream._
    //別の実装。foldはNoneとSomeの両方のパターンにそれぞれ関数を適用するだけだから実質同じ
    def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).fold(empty[A])((p: (A, S)) => cons(p._1, unfold(p._2)(f)))
    assert(
      unfoldViaFold(3)(n => Some((n, n + 1)))
        .take(5)
        .toList == List(3, 4, 5, 6, 7))

    //別の実装２。これもgetOrElseでNoneのパターンをカバーしているだけなので実質同じ
    def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])
    assert(
      unfoldViaMap(3)(n => Some((n, n + 1)))
        .take(5)
        .toList == List(3, 4, 5, 6, 7))
  }

  test("Ex 5.12 unfoldを使ってfibs, from, constant, onesを記述せよ。") {
    //再掲
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, s)) => Stream.cons(h, unfold(s)(f))
      case None         => Stream.empty
    }

    def fibsViaUnfold(): Stream[Int] =
      unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

    assert(fibsViaUnfold().take(5).toList == List(0, 1, 1, 2, 3))

    def fromViaUnfold(n: Int): Stream[Int] =
      unfold(n)(n => Some((n, n + 1)))

    assert(fromViaUnfold(1).take(5).toList == List(1, 2, 3, 4, 5))

    def constantViaUnfold[A](a: A): Stream[A] =
      unfold(a)(_ => Some((a, a)))

    assert(constantViaUnfold(3).take(5).toList == List(3, 3, 3, 3, 3))

    def onesViaUnfold(): Stream[Int] =
      unfold(1)(_ => Some((1, 1)))

    assert(onesViaUnfold().take(3).toList == List(1, 1, 1))
  }

  test(
    "Ex 5.13 unfoldを使って（第3章で示したような）map, take, takeWhile, zipWith, zipAllを実装せよ。zipAll関数では、どちらかのストリームに要素が残っている限り、評価を続ける必要がある。この関数はストリームが完全に評価されたかどうかを示すのにOptionを使用する。") {
    //実装はStream.scalaを参照
    //TODO テスト

  }

  test(
    "Ex 5.14 難問：これまで記述してきた関数を使ってstartsWithを実装せよ。この関数は、あるStreamが別のStreamのプレフィックス(接頭辞)であるかどうかを調べる。たとえば、`Stream(1, 2, 3) startsWith Stream(1, 2)`の結果はtrueになる。") {

    //実装はStream.scalaを参照
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  }

  test(
    "Ex 5.15 unfoldを使ってtailsを実装せよ。与えられたStreamに対し、tailsは元のStreamから始まる入力シーケンスのサフィックス（接尾辞）であるStreamを返す。たとえばStream(1, 2, 3)が与えられた場合は、Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream())を返す。") {
    //実装はStream.scalaを参照

    //TODO テスト Stream同士の比較はどうやる？
    /* これはテストを通らない
    assert(
      Stream(1, 2, 3).tails ==
        Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream())
    )

    Expected :Cons(fpinscala.chapter05.Stream$$$Lambda$157/1742920067@16aa0a0a,fpinscala.chapter05.Stream$$$Lambda$158/1564984895@780cb77)
    Actual   :Cons(fpinscala.chapter05.Stream$$$Lambda$157/1742920067@5ef60048,fpinscala.chapter05.Stream$$$Lambda$158/1564984895@1d548a08)
   */
  }

  test(
    "Ex 5.16 難問：tailsをscanRight関数として一般化せよ。foldRightと同様に、この関数は中韓結果のストリームを返す。"
  ) {
    //実装はStream.scalaを参照
    //TODO よく分かってない
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }
}
