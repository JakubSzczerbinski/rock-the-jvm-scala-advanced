package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {
  val aCondition : Boolean = false
  val aConditionVal = if (aCondition) 42 else 65
  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  val theUnit : Unit = println("Hello, Scala!")
  def aFunction(x: Int) : Int = x + 1
  def factorial(n : Int) = {
    @tailrec
    def aux(n: Int, acc: Int): Int =
      if (n <= 0) acc else aux(n - 1, n * acc)
    aux(n, 1)
  }

  class Animal
  class Dog extends Animal
  val aDog : Animal = new Dog

  trait Carnivore {
    def eat(a : Animal) : Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch")
  }

  val aCroc = new Crocodile
  aCroc eat aDog

  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("mnomnomnomnomnom!")
  }

  abstract class MyList[+A]
  object MyList

  case class Person(name : String, age : Int)

  val throwsException = throw new RuntimeException
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e : Exception => "I caught an exception"
  } finally {
    println("Finally XD")
  }
  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x : Int) => x + 1
  List(1, 2, 3).map(anonymousIncrementer)

  val pairs = for {
    num <- List(1, 2, 3)
    char <- List('a', 'b', 'c') if num < 3
  } yield num + " " + char

  val aMap = Map(
    "Daniel" -> 5,
    "Jess" -> 3
  )

  val opt : Option[Int] = Some(2)
  val x = 2
  val ordering = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 25)
  val greeting = bob match {
    case Person(name, _) => s"Hi, $name!"
  }
}
