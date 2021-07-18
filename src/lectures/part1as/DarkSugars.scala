package lectures.part1as

import scala.util.Try

object DarkSugars extends App {
  def singleArgMethod(arg : Int) : String = s"$arg little ducks..."
  val description = singleArgMethod {
    2
  }

  val aTryInstance = Try {
    throw new RuntimeException
  }

  List(1, 2, 3) map { x =>
    x + 1
  }
  // single abstract method
  trait Action {
    def act(x : Int) : Int
  }
  val anInstance : Action = new Action {
    override def act(x: Int): Int = x + 1
  }
  val aFunkyInstance : Action = (x : Int) => x + 1

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("XD")
  })
  val aBetterThread = new Thread(() => println("The scala way!!!"))

  abstract class AbsType {
    def impl: Int = 1
    def f(a : Int) : Unit
  }

  val absInstance : AbsType = (a :Int) => println(s"Sweet XD")

  val prependedList = 2 :: List(3, 4)

  trait --> [A, B] {
    def run(a : A) : B
  }
  val x : Int --> String = (a : Int) => { s"This is int $a" }
  println(x.run(3))

  class FancyName (a : Int) {
    def `print the value of int`(print : String => Unit) : Unit = print(a.toString)
  }
  val y = new FancyName(2)
  y `print the value of int` println

  class Mutable[A](value : A) {
    private var internalMember : A = value
    def member : A = internalMember
    def member_=(value : A) : Unit = {
      internalMember = value
    }
    def print() : Unit = println(s"Mutable value=$member")
  }

  val mutable = new Mutable(1)
  mutable.print()
  mutable.member = 10
  mutable.print()
}
