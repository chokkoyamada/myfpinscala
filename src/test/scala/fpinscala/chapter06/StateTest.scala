package fpinscala.chapter06

import fpinscala.chapter06.RNG._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ListBuffer

class StateTest extends FunSuite with Matchers {
  test("同じSeedのSimpleRNGは常に同じ値を返し、参照透過性を保っている") {
    val rng = SimpleRNG(42)

    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
    assert(rng2 == SimpleRNG(1059025964525L))

    val (n2, rng3) = rng2.nextInt
    assert(n2 == -1281479697)
    assert(rng3 == SimpleRNG(197491923327988L))
  }

  test(
    "EX 6.1 RNG.nextIntを使って0~Int.maxValue(0とInt.maxValueを含む)のランダムな整数を生成する関数を記述せよ。なお、nextIntがInt.minValueを返すときには、対応する自然数がない。この特異なケースにも対処する必要がある。") {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (a, b) = rng.nextInt
      (if (a < 0) -(a + 1) else a, b) //+1することでInt.minValueのケースに対応している
    }
    val rng = SimpleRNG(42)
    assert(nonNegativeInt(rng) == (16159453, SimpleRNG(1059025964525L)))
    val (_, rng2) = rng.nextInt
    assert(nonNegativeInt(rng2) == (1281479696, SimpleRNG(197491923327988L)))
  }

  test(
    "Ex 6.2 0~1(1を含まない)のDouble型の値を生成する関数を記述せよ。Int.MaxValueを使って正の整数の最大値を取得できることと、x.toDoubleを使ってx: IntをDoubleに変換できることに注意。") {
    def double(rng: RNG): (Double, RNG) = {
      val (a, r) = nonNegativeInt(rng)
      (a / Int.MaxValue.toDouble + 1, r)
    }

    val rng = SimpleRNG(10000)
    assert(double(rng) == (1.2083733185233423, SimpleRNG(252149039170011L)))
  }

  test(
    "Ex 6.3 ペア(Int, Double) ペア(Double, Int), および三要素のタプル(Double, Double, Double)を生成する関数を記述せよ。すでに作成済みの関数を再利用できるはずだ。") {
    val rng = SimpleRNG(42)

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng1) = rng.nextInt
      val (d, rng2) = double(rng1)
      ((i, d), rng2)
    }

    assert(
      intDouble(rng) == ((16159453, 0.5967354848980904), SimpleRNG(
        197491923327988L)))

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (d, rng1) = double(rng)
      val (i, rng2) = rng1.nextInt
      ((d, i), rng2)
    }

    assert(
      doubleInt(rng) == ((0.007524831686168909, -1281479697), SimpleRNG(
        197491923327988L)))

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = double(rng)
      val (d2, rng2) = double(rng1)
      val (d3, rng3) = double(rng2)
      ((d1, d2, d3), rng3)
    }
    assert(
      double3(rng) == ((0.007524831686168909,
                        0.5967354848980904,
                        0.15846728393808007), SimpleRNG(259172689157871L)))
  }

  test("Ex 6.4 ランダムな整数のリストを生成する関数を記述せよ。") {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count == 0) {
        (Nil, rng)
      } else {
        var l = ListBuffer.empty[Int]
        var r = rng
        for (c <- 1 to count) {
          val (x, y) = r.nextInt
          l.append(x)
          r = y
        }
        (l.toList, r)
      }
    }

    val rng = SimpleRNG(42)
    assert(
      ints(5)(rng) == (List(16159453,
                            -1281479697,
                            -340305902,
                            -2015756020,
                            1770001318), SimpleRNG(115998806404289L)))
  }

  test("Ex 6.5 mapを使ってdoubleをもう少し要領よく実装しなおせ。Ex 6.2を参照。") {
    def double(rng: RNG): Rand[Double] = {
      map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)
    }

    val rng = SimpleRNG(10000)
    assert(
      double(rng).apply(rng) == (1.2083733185233423, SimpleRNG(
        252149039170011L)))
    //なぜapplyしなおさないと値が取れないのか？
  }

  test(
    "Ex 6.6 以下のシグネチャにもとづいてmap2を実装せよ。この関数は、raとrbの2つのアクションと、それらの結果を結合する関数fを受取り、それらを結合する新しいアクションを返す。") {
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng =>
        {
          val (a, rng2) = ra(rng)
          val (b, rng3) = rb(rng2)
          (f(a, b), rng3)
        }
    }

    val rng: SimpleRNG = SimpleRNG(0)
    val ra: Rand[Int] = unit(1)
    val rb: Rand[Int] = unit(2)
    assert(map2(ra, rb)((a, b) => a + b).apply(rng) == unit(3).apply(rng))
  }

  test(
    "Ex 6.7 2つのRNG遷移の組み合わせが可能であるとしたら、それらのリスト全体を結合することも可能であるはずだ。遷移のリストを1つの遷移にまとめるためのsequenceを実装せよ。それを使って、以前に記述したints関数を再実装せよ。その際には、標準ライブラリのList.fill(n)(x)関数を使ってxをn回繰り返すリストを作成できる。") {
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))

    val rng: SimpleRNG = SimpleRNG(0)
    val ra: Rand[Int] = unit(1)
    val rb: Rand[Int] = unit(2)
    val rc: Rand[Int] = unit(3)
    val rd: Rand[Int] = unit(4)
    val re: Rand[Int] = unit(5)

    assert(
      sequence(List(ra, rb, rc, rd, re)).apply(rng)
        == (List(1, 2, 3, 4, 5), rng))
  }

  test("Ex 6.8 flatMapを実装し、それを使ってnonNegativeLessThanを実装せよ。") {
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }

    val rng = SimpleRNG(0)
    val ra = unit(1)
    assert(flatMap(ra)(a => unit(a + 1)).apply(rng) == unit(2).apply(rng))

    def flatMap2[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        g(f(rng)._1)(f(rng)._2)
      }
    assert(flatMap2(ra)(a => unit(a + 1)).apply(rng) == unit(2).apply(rng))

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    //違ったrngを与えるとそれぞれ結果が一意に決まる
    val rng2 = SimpleRNG(5)
    assert(
      nonNegativeLessThan(10).apply(rng2)
        == unit(4).apply(SimpleRNG(126074519596L)))
    assert(
      nonNegativeLessThan(99).apply(rng2)
        == unit(75).apply(SimpleRNG(126074519596L)))
    val rng3 = SimpleRNG(12)
    assert(
      nonNegativeLessThan(10).apply(rng3)
        == unit(6).apply(SimpleRNG(302578847015L)))
    assert(
      nonNegativeLessThan(99).apply(rng3)
        == unit(22).apply(SimpleRNG(302578847015L)))
  }

  test(
    "Ex 6.9 flatMapを使ってmapとmap2を再実装せよ。これが可能であることは、flatMapがmapとmap2よりも強力であると述べていることから明らかである。") {
    //モナドへの布石
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    val rng = SimpleRNG(10)
    val ra = unit(14)
    assert(map(ra)(_ * 2).apply(rng) == unit(28).apply(rng))

    val rb = unit(18)
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(a => map(rb)(b => f(a, b)))
    }
    assert(map2(ra, rb)(_ + _).apply(rng) == unit(32).apply(rng))
  }

  test(
    "Ex 6.10 unit, map, map2, flatMap, sequenceの5つの関数を一般化せよ。可能であれば、それらをStateケースクラスのメソッドとして追加せよ。それが不可能であれば、Stateコンパニオンオブジェクトに配置せよ。") {
    import State.unit //RNG.unitを使ってしまうため

    case class State[S, +A](run: S => (A, S)) {
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State(s => {
          val (a, s1) = run(s)
          f(a).run(s1)
        })

      def map[B](f: A => B): State[S, B] =
        flatMap(a => unit(f(a)))

      def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(a => sb.map(b => f(a, b)))

      def map2_[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        for {
          a1 <- this
          b1 <- sb
        } yield f(a1, b1)
    }

    object State {
      type Rand[A] = State[RNG, A]

      def unit[S, A](a: A): State[S, A] = State(s => (a, s))

      def sequenceViaFoldRight[S, A](
          sas: List[State[S, A]]): State[S, List[A]] =
        sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

      def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
        def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
          actions match {
            case Nil => (acc.reverse, s)
            case h :: t =>
              h.run(s) match {
                case (a, s2) => go(s2, t, a :: acc)
              }
          }

        State((s: S) => go(s, sas, List()))
      }
    }

    val rng = SimpleRNG(8)
    val rng2 = SimpleRNG(9)
    val ra: State[RNG, Int] = State(r => r.nextInt)
    val rb: State[RNG, Double] =
      State[RNG, Int](r => r.nextInt).map(x => x.toDouble)
    val rc: State[RNG, Int] = State(r => nonNegativeInt(r))
    val rd: State[RNG, Int] = State(r => nonNegativeInt(r)).map(_ + 2)
    val re: State[RNG, Int] = State(r => nonNegativeInt(r)).map(_ * 3)

    //mapのテスト。 生成される値は当然別だが、状態は同じである
    assert(ra.map(_ * 3).run(rng) == (9233973, SimpleRNG(201719231347L)))
    assert(ra.map(_ + 4).run(rng) == (3077995, SimpleRNG(201719231347L)))

    //unitはコンテキストをつくるだけで、状態を変化させない
    assert(unit(4).run(rng) == (4, SimpleRNG(8)))
    assert(unit(6).run(rng) == (6, SimpleRNG(8)))

    //map2
    assert(
      ra.map2(rb)(_ + _).run(rng) == (-6.46631787E8, SimpleRNG(
        238895596714498L)))
    assert(
      ra.map2(rb)(_ + _).run(rng2) == (-1.801731614E9, SimpleRNG(
        163169759544427L)))

    //flatMapのテスト
    assert(
      ra.flatMap(a => unit(a + 1)).run(rng) == (3077992, SimpleRNG(
        201719231347L)))
    assert(
      ra.flatMap(a => unit(a * 2)).run(rng) == (6155982, SimpleRNG(
        201719231347L)))
    assert(
      ra.flatMap(a => unit(a + 1)).run(rng2) == (3462741, SimpleRNG(
        226934135264L)))

    //sequenceのテスト
    assert(
      State.sequence(List(ra, rb, rc, rd, re)).run(rng)
        == (List(3077991, -6.49709778E8, 1351730011, 996922074, -1746628465),
        SimpleRNG(225805665494183L)))
  }

  test(
    "Ex 6.11 Stateの使用に慣れるために、単純なスナックの自動販売機をモデリングする有限状態オートマトンを実装せよ。この自動販売機では、2種類の入力を使用する。すなわち、効果を投入することができ、ハンドルを回してスナックを取り出すことができる。自動販売機はロックされた状態とロックが解除された状態のどちらかになる。また、残りのスナックの数と自動販売機に投入された硬貨の数も追跡する。") {

    object Candy {
      def update(i: Input)(s: Machine): Machine =
        (i, s) match {
          case (_, Machine(_, 0, _))        => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _))  => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        }

      def simulateMachine(inputs: List[Input]): (Int, Int) = {
        var machine = Machine(true, 5, 10)
        inputs.foreach(i => machine = update(i)(machine))
        (machine.coins, machine.candies)
      }
      /*
      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
        val l = for {
          input <- inputs
        } yield modify[Machine](update(input))
        for {
          _ <- State.sequence(l)
          s <- get
        } yield (s.coins, s.candies)
      }
     */
    }

    Candy.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)) shouldBe (14, 1)
  }

  test("State.getについて") {
    import fpinscala.chapter06.State._
    //状態がrunされれば値を返す　
    get.run(1) shouldBe (1, 1)
    get[Int].run(2) shouldBe (2, 2)

    case class Machine(locked: Boolean, candies: Int, coins: Int)
    get.run(Machine(false, 2, 3)) shouldBe (Machine(false, 2, 3), Machine(false,
                                                                          2,
                                                                          3))
  }
}
