package lectures.part2afp

import javax.sql.rowset.Predicate
import scala.collection.View.Empty

object LazyEvaluation extends App {
  lazy val x : Int = {
    println("hello")
    42
  }
  println(x)
  println(x)
  def sideEffectCond : Boolean = {
    println("Boo")
    true
  }

  def simpleCondition : Boolean = false

  lazy val lazyCondition = sideEffectCond
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  def byNameMethod(n : => Int) = {
    lazy val t = n // CALL BY NEED
    t + t + t + 3
  }

  def computeMagicValue : Int = {
    println("Computing")
    Thread.sleep(1000)
    42
  }

  byNameMethod(computeMagicValue)

  def lessThan30(i : Int) : Boolean = {
    println(s"$i < 30")
    i < 30
  }

  def greaterThan20(i : Int) : Boolean = {
    println(s"$i > 20")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  gt20lazy.foreach(println)


}


