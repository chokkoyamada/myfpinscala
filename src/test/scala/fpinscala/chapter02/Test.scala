package fpinscala.chapter02

import org.scalatest.FunSuite

class Test extends FunSuite {
  test("Ex 2.1 n番目のフィボナッチ数を種痘する再帰関数を記述せよ。再帰関数の定義では、ローカルな末尾再帰関数を使用すること。") {
    def fib(n: Int): Int = {
      n match {
        case 0 => 0
        case 1 => 0
        case 2 => 1
        case _ => fib(n - 2) + fib(n - 1)
      }
    }

    assert(fib(1) == 0)
    assert(fib(2) == 1)
    assert(fib(3) == 1)
    assert(fib(4) == 2)
    assert(fib(5) == 3)
    assert(fib(6) == 5)
    assert(fib(7) == 8)
  }

  test("Ex 2.2 指定された比較関数に従ってArray[A]がソートされているかどうかを調べるisSortedを実装せよ。") {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @scala.annotation.tailrec
      def loop(n: Int): Boolean = {
        if (as.isEmpty) true
        else n match {
          case x if x >= as.length-1 => true
          case x if ordered(as(n), as(n+1)) => loop(n+1)
          case _ => false
        }
      }
      loop(0)
    }
    assert(isSorted(Array(1, 3, 4, 5, 9), (x: Int, y: Int) => x < y))
    assert(!isSorted(Array(8, 3, 2, 5, 9), (x: Int, y: Int) => x < y))
    assert(isSorted(Array(0.4, 1.4, 6, 9.43, 13), (x: Double, y: Double) => x < y))
  }

  test("Ex 2.3 カリー化(currying)では、引数2つの関数fが、fを部分的に適用する引数1つの関数に変換される。この場合も、コンパイルできる実装は１つだけである。この実装を記述せよ。") {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      (a: A) => (b: B) => f(a, b)
    }
  }

  test("Ex 2.4 curryによる変換を逆向きに行うuncurryを実装せよ。=>は右結合であるため、A => (B => C)はA => B => Cと記述できる。") {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }
  }

  test("Ex 2.5 2つの関数を合成する高階関数を実装せよ。") {
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }
  }

}
