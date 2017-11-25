package fpinscala.chapter04

import org.scalatest.FunSuite

class Test extends FunSuite {
  test(
    "Ex 4.1 リスト4-4のすべての関数をOptionで実装せよ。各関数を実装するときに、その関数の意味と、それを使用するであろう状況について考えること。どの関数をいつ使用するかについては、後ほど考察する。") {
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case None    => None
        case Some(x) => Some(f(x))
      }
      def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None    => None
        case Some(x) => f(x)
      }
      def flatMap2[B](f: A => Option[B]): Option[B] =
        map(f).getOrElse(None)

      def getOrElse[B >: A](default: => B): B =
        this match { // `B >: A` は、Bの型がAの型に等しいか、Aのスーパークラスであること
          case None    => default
          case Some(x) => x
        }
      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _    => this
      }
      def filter(f: A => Boolean): Option[A] = this match {
        case Some(x) if f(x) => this
        case _               => None
      }
      def filter2(f: A => Boolean): Option[A] =
        flatMap(a => if (f(a)) Some(a) else None)

    }
    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]
  }

  test(
    "Ex 4.2 flatMapをベースとして、variance関数を実装せよ。シーケンスの平均をm, シーケンスの各要素をxとすれば、分散はmath.pow(x - m, 2)の平均となる。") {
    import Option._
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    assert(variance(Seq(90.0, 100.0, 110.0)) == Some(66.66666666666667))
    assert(variance(Seq(77.0, 80.0, 83.0)) == Some(6))
  }

  test(
    "Ex 4.3 二項関数を使ってOption型の2つの値を統合する総称関数map2を記述せよ。どちらかのOption値がNoneの場合は、戻り値もNoneになる。") {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)

    def map2_[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap (aa => b.map(bb => f(aa, bb)))

    assert(map2(Some(2), Some(4))((a, b) => a + b) == Some(6))
    assert(map2(Some(2), None)((a, b) => a + b) == None)

    assert(map2_(Some(2), Some(4))((a, b) => a + b) == Some(6))
    assert(map2_(Some(2), None)((a, b) => a + b) == None)
  }

  test(
    "Ex 4.4 Optionのリストを1つのOptionにまとめるsequence関数を記述せよ。新しいOptionには、元のリストに含まれるすべてのSome値のリストが含まれる。元のリストにNoneが1つでも含まれていた場合、この関数の結果はNoneになる。それ以外の場合は、全ての値のリストを含んだSomeになる。シグネチャは以下の通り。") {
    import Option._
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

    //TODO flatMap + mapの組み合わせが浮かばない。forにすればまだ分かるが...
    def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap (hh => sequence2(t).map(hh :: _))
    }

    // sequence2を単にforシンタックスで書き直したもの
    def sequence3[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t =>
        for {
          hh <- h
          tt <- sequence3(t)
        } yield hh :: tt
    }

    assert(sequence(List(Some(2), Some(3), Some(9))) == Some(List(2, 3, 9)))
    assert(sequence(List(Some(2), None, Some(9))) == None)
  }

  test(
    "Ex 4.5 このtraverse関数を実装せよ。mapとsequenceを使用すれば簡単だが、リストを1回だけ調べる、より効率の良い実装にすること。要するに、traverseの観点からsequenceを実装すれば良い。") {
    import Option._
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil    => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      }
    def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(x => x)

    assert(traverse(List(2, 3, 9))(x => Some(x * 2)) == Some(List(4, 6, 18)))
    assert(
      traverse(List("a", "", "c"))(s => if (s.isEmpty) None else Some(s)) == None)
  }

  test("Ex 4.6 Right値を操作するmap, flatMap, orElse, map2をEitherに追加せよ。") {
    assert(Right(2).map(_ * 2) == Right(4))
    assert(Left(2).map(x => x) == Left(2)) //TODO Rightの型アノテーションするにはどうすればいい？
  }
}
