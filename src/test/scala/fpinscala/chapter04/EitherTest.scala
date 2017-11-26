package fpinscala.chapter04

import org.scalatest.FunSuite

class EitherTest extends FunSuite {

  test("Ex 4.6 Right値を操作するmap, flatMap, orElse, map2をEitherに追加せよ。") {
    assert(Right(2).map(_ * 2) == Right(4))
    assert(Left(2).map(x => x) == Left(2)) //TODO Rightの型アノテーションするにはどうすればいい？
  }

  test(
    "Ex 4.7 Eitherでsequenceとtraverseを実装せよ。これらは、エラーが発生した場合に、最初に検出したエラーを返すものとする。") {
    def traverse[E, A, B](es: List[A])(
        f: A => Either[E, B]): Either[E, List[B]] = es match {
      case Nil    => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

    assert(
      traverse(List(1, 2, 3, 4, 5))(a => if (a < 5) Right(a) else Left(a)) == Left(
        5))
    assert(
      traverse(List(1, 2, 3, 4, 5))(a => if (a < 6) Right(a) else Left(a)) == Right(
        List(1, 2, 3, 4, 5)))

    //foldRightを使ったバージョン
    def traverse2[E, A, B](es: List[A])(
        f: A => Either[E, B]): Either[E, List[B]] =
      es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) =>
        f(a).map2(b)(_ :: _))

    assert(
      traverse2(List(1, 2, 3, 4, 5))(a => if (a < 5) Right(a) else Left(a)) == Left(
        5))
    assert(
      traverse2(List(1, 2, 3, 4, 5))(a => if (a < 6) Right(a) else Left(a)) == Right(
        List(1, 2, 3, 4, 5)))

    //sequenceはtraverseの特殊ケース
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(x => x)

    assert(
      sequence(List(Right(1), Right(2), Right(3), Right(4), Right(5)))
        == Right(List(1, 2, 3, 4, 5)))
    assert(
      sequence(List(Right(1), Right(2), Right(3), Left(4), Right(5)))
        == Left(4))

  }
}
