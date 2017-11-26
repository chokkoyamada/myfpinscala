package fpinscala.chapter07

import org.scalatest.{FunSuite, Matchers}
import fpinscala.parallelism.Par.Par
import java.util.concurrent._

/**
  * @author yamadanaoyuki
  */
class Test extends FunSuite with Matchers {
  test("リスト7-1") {
  }
  test("Ex 7.1 Par.map2は２つの並列計算の結果を結合する新しい高階関数である。そのシグネチャはどのようなものになるか。Intにのみ対応すると想定せず、できるだけ汎用的なシグネチャを示せ。"){
    /* def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B)=>C): Par[C] = ??? */
  }
  test("Ex 7.3 map2の実装を修正し、Futureのタイムアウトの規約に従うようにせよ。") {
    case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B)=>C) extends Future[C] {
      @volatile var cache: Option[C] = None
      def isDone = cache.isDefined
      def isCancelled = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      def get = compute(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime()
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime; val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }
    def map2[A, B, C](a: Par[A], b:Par[B])(f: (A,B) =>C): Par[C] =
      es => {
        val(af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
      }
  }
}
