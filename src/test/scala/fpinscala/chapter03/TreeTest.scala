package fpinscala.chapter03

import org.scalatest.FunSuite

/**
  * @author yamadanaoyuki
  */
class TreeTest extends FunSuite {
  test("Ex 3.25 2分木のノード(LeafとBranch)の数を数えるsize関数を記述せよ。") {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(1))) == 3)
    assert(size(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 5)
  }

  test(
    "Ex 3.26 Tree[Int]の最大の要素を返すmaximum関数を記述せよ。なおScalaでは、x.max(y) または x max y を使って2つの整数xとyの最大値を計算できる。") {
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x)      => x
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

    assert(maximum(Leaf(1)) == 1)
    assert(maximum(Branch(Leaf(2), Leaf(1))) == 2)
    assert(maximum(Branch(Leaf(2), Branch(Leaf(1), Leaf(3)))) == 3)
  }

  test("Ex 3.27 2分木のルートから任意のLeafまでの最長パスを返すdepth関数を記述せよ。") {
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
    assert(depth(Leaf(1)) == 0)
    assert(depth(Branch(Leaf(2), Leaf(1))) == 1)
    assert(depth(Branch(Leaf(2), Branch(Leaf(1), Leaf(3)))) == 2)
  }

  test("Ex 3.28 2分木の各要素を特定の関数を使って変更するmap関数を記述せよ。この関数はListの同じ名前のメソッドに類似している。") {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x)      => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    assert(map(Leaf(1))(_ * 2) == Leaf(2))
    assert(map(Branch(Leaf(2), Leaf(1)))(_ * 2) == Branch(Leaf(4), Leaf(2)))
    assert(
      map(Branch(Leaf(2), Branch(Leaf(1), Leaf(3))))(_ * 2) == Branch(
        Leaf(4),
        Branch(Leaf(2), Leaf(6))))
  }

  test(
    "Ex 3.29 size, maximum, depth, mapを一般化し、それらの類似点を抽象化する新しいfold関数を記述せよ。そして、このより汎用的なfold関数を使ってそれらを再実装せよ。このfold関数とListの左畳み込みおよび右畳み込みの間にある類似性を抽出することは可能か。") {
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    //TODO
    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(a => 1)(1 + _ + _)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(a => 0)((l, r) => 1 + (l max r))

    //TODO
    def mapViaFild[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _)) //型アノテーションが必要。本家のanswerのコメント参照
  }
}
