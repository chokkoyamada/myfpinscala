package fpinscala.chapter_06

import fpinscala.state.RNG
import fpinscala.state.RNG._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ListBuffer

class Test extends FunSuite with Matchers {
  test("同じSeedのSimpleRNGは常に同じ値を返し、参照透過性を保っている") {
    val rng = SimpleRNG(42)

    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
    assert(rng2 == SimpleRNG(1059025964525L))

    val (n2, rng3) = rng2.nextInt
    assert(n2 == -1281479697)
    assert(rng3 == SimpleRNG(197491923327988L))
  }

  test("EX 6.1 RNG.nextIntを使って0~Int.maxValue(0とInt.maxValueを含む)のランダムな整数を生成する関数を記述せよ。なお、nextIntがInt.minValueを返すときには、対応する自然数がない。この特異なケースにも対処する必要がある。"){
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (a, b) = rng.nextInt
      if(a<0) (-a+1, b) else (a, b)
    }
  }

  test("Ex 6.2 0~1(1を含まない)のDouble型の値を生成する関数を記述せよ。Int.MaxValueを使って正の整数の最大値を取得できることと、x.toDoubleを使ってx: IntをDoubleに変換できることに注意。") {
    def double(rng: RNG): (Double, RNG) = {
      val (a, b) = rng.nextInt
      ((a/Int.MaxValue).toDouble, b)
    }
  }

  test("Ex 6.3 ペア(Int, Double) ペア(Double, Int), および三要素のタプル(Double, Double, Double)を生成する関数を記述せよ。すでに作成済みの関数を再利用できるはずだ。"){
    def double(rng: RNG): (Double, RNG) = {
      val (a, b) = rng.nextInt
      ((a/Int.MaxValue).toDouble, b)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (a, b) = rng.nextInt
      val (c, d) = double(b)
      ((a, c), d)
    }
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (a, b) = double(rng)
      val (c, d) = b.nextInt
      ((a, c), d)
    }
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (a, b) = double(rng)
      val (c, d) = double(b)
      val (e, f) = double(d)
      ((a, c, e), f)
    }
  }

  test("Ex 6.4 ランダムな整数のリストを生成する関数を記述せよ。") {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if(count == 0) {
        (Nil, rng)
      }else {
        var l = ListBuffer.empty[Int]
        var r = rng
        for(c <- 1 to count){
          val (x, y) = r.nextInt
          l.append(x)
          r = y
        }
        (l.toList, r)
      }
    }
  }

  test("Ex 6.5 mapを使ってdoubleをもう少し要領よく実装しなおせ。Ex 6.2を参照。"){
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (a, b) = rng.nextInt
      if(a<0) (-a+1, b) else (a, b)
    }

    def double(rng: RNG): Rand[Double] = {
      map(nonNegativeInt)(i => (i / Int.MaxValue).toDouble)
    }

  }

  test("Ex 6.6 以下のシグネチャにもとづいてmap2を実装せよ。この関数は、raとrbの2つのアクションと、それらの結果を結合する関数fを受取り、それらを結合する新しいアクションを返す。") {
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }
    }

    def map22[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {

        ( f(ra(rng)
      }
    }
  }

  test("Ex 6.7 2つのRNG遷移の組み合わせが可能であるとしたら、それらのリスト全体を結合することも可能であるはずだ。遷移のリストを1つの遷移にまとめるためのsequenceを実装せよ。それを使って、以前に記述したints関数を再実装せよ。その際には、標準ライブラリのList.fill(n)(x)関数を使ってxをn回繰り返すリストを作成できる。"){
    def sequence[A](fs: List[Rand[A]]): Rand[List[A ]] = ???
  }

  test("Ex 6.8 flatMapを実装し、それを使ってnonNegativeLessThanを実装せよ。"){}


  test("Ex 6.9 flatMapを使ってmapとmap2を再実装せよ。これが可能であることは、flatMapがmapとmap2よりも強力であると述べていることから明らかである。"){}


  test("Ex 6.10 unit, map, map2, flatMap, sequenceの5つの関数を一般化せよ。可能であれば、それらをStateケースクラスのメソッドとして追加せよ。それが不可能であれば、Stateコンパニオンオブジェクトに配置せよ。"){}


  test("Ex 6.11 Stateの使用に慣れるために、単純なスナックの自動販売機をモデリングする有限状態オートマトンを実装せよ。この自動販売機では、2種類の入力を使用する。すなわち、効果を投入することができ、ハンドルを回してスナックを取り出すことができる。自動販売機はロックされた状態とロックが解除された状態のどちらかになる。また、残りのスナックの数と自動販売機に投入された硬貨の数も追跡する。"){
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input
    case class Machine(locked: Boolean, candies: Int, coins: Int)
  }


}
