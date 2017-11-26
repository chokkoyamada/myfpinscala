package fpinscala.chapter08

import fpinscala.chapter06.{RNG, State}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author yamadanaoyuki
  */
class Test extends FunSuite with Matchers {
  test("Ex 8.3") {
    trait Prop {
      def check: Boolean

      def &&(p: Prop): Prop = new Prop{
        def check: Boolean = Prop.this.check && p.check
      }
    }
  }

  test("Ex 8.4 Genのこの表現を使って、startからstopExclusiveの範囲内の整数を生成するGen.chooseを実装せよ。既に記述してある関数を自由に使用して構わない。") {
    case class Gen[A](sample: State[RNG, A])

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

    def choose2(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(rng => RNG.nonNegativeInt(rng) match {
        case(n, rng2) => (start + n % (stopExclusive-start), rng2)
      }))
  }

  test("Ex 8.5 Genのこの表現を使って他に何を実装できるか。試しにunit, boolean, listOfNを実装せよ。unitは常にaの値を生成し、listOfNはジェネレータgを使って長さnのリストを生成する。") {
    case class Gen[A](sample: State[RNG, A])
    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))
    def boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))
  }
  test("Ex 8.6 "){}

}
