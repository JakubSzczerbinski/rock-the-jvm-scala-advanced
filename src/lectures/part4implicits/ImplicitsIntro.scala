package lectures.part4implicits

object ImplicitsIntro extends App {

  val pair = "Daniel" -> "555"
  val intPair = 1 -> 2

  case class Person(name : String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def personOfString(str : String) : Person = Person(str)

  println("Peter".greet)

  def increment(x : Int)(implicit amount : Int) : Int = x + amount
  implicit val defaultAmount : Int = 10

  println(increment(2))


}
