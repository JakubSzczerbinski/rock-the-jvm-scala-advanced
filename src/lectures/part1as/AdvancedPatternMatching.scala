package lectures.part1as

import scala.annotation.tailrec

object AdvancedPatternMatching extends App {
  def reminder() : Unit = {
    val numbers = List(1)
    val description: String = numbers match {
      case h :: tl => h.toString
      case _ => "-"
    }
    println(description)
  }

  def advanced() : Unit = {
    class Person(val name : String, val age : Int)

    object Person {
      def unapply(person: Person) : Option[(String, Int)] = Some((person.name, person.age))
      def unapply(age : Int) : Option[String] = Some(if (age < 18) "minor" else "major")
    }

    val bob = new Person("Bob", 23)
    val greeting = bob match {
      case Person(name, age) => s"Hi, my name is $name and I'm $age years old."
    }
    println(greeting)

    val legalStatus = bob.age match {
      case Person(status) => status
    }
    println(legalStatus)
  }

  def conditions() : Unit = {
    object odd {
      def unapply(x : Int) : Option[Int] =
        if (x % 2 == 1) Some(x) else None
    }

    object even {
      def unapply(x : Int) : Option[Int] =
        if (x % 2 == 0) Some(x) else None
    }

    object single_digit {
      def unapply(x : Int) : Option[Int] =
        if (x < 10 && x >= 0) Some(x) else None
    }

    val number : Int = 16
    val numberProperty = number match {
      case single_digit(n) => s"$n is single digit"
      case odd(n) => s"$n is odd"
      case even(n) => s"$n is even"
    }
    println(numberProperty)
  }

  def infix() : Unit = {
    case class Or[A, B](a : A, b : B)
    val either = Or(2, "two")
    val humanDescription = either match {
      case number Or string => s"$number is written as $string"
    }
    println(humanDescription)
  }

  def seq() : Unit = {
    val vararg = List(1, 2, 3) match {
      case List(1, _*) => s"starting with 1"
    }

    abstract class MyList[+A] {
      def head : A = ???
      def tail : MyList[A] = ???
    }
    case object Empty extends MyList[Nothing]
    case class Cons[+A](override val head: A, override val tail : MyList[A]) extends MyList[A]
    object MyList {
      def unapplySeq[A](list : MyList[A]) : Option[Seq[A]] =
        if (list == Empty) Some(Seq.empty)
        else unapplySeq(list.tail) map (list.head +: _)
    }

    val ml : MyList[Int] = Cons(0, Cons(2, Cons(3, Empty)))
    val decompose = ml match {
      case MyList(1, 2, _*) => "starting with 1, 2"
      case _ => "something else"
    }
    println(decompose)
  }

  reminder()
  advanced()
  conditions()
  infix()
  seq()
}
