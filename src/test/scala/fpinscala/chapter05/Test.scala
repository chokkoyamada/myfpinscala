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

  test("Ex 5.7 foldRightを使ってmap, filter, append, flatMapを実装せよ。appendメソッドはその引数に対して非正格でなければならない。") {
    
  }
}
