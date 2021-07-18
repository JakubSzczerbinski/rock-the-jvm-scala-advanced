package lectures.part2afp

object PartialFunctions extends App {
  val aFunction : Int => Int = (x : Int) => x + 1
  val aFussyFunction : Int => Int = (x : Int) =>
    if (x == 42) 42
    else if (x == 2) 56
    else if (x == 5) 32
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction : Int => Int = (x : Int) => x match {
    case 42 => 42
    case 2 => 56
    case 5 => 32
  }

  val aPartialFunction : PartialFunction[Int, Int] = {
    case 42 => 42
    case 2 => 56
    case 5 => 32
  }

  println(aPartialFunction(2))
  println(aPartialFunction.isDefinedAt(214))

  val lifted = aPartialFunction.lift
  println(lifted(2))
  println(lifted(214))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(pfChain(2))
  println(pfChain(45))

  val aTotalFunction : Int => Int = {
    case 1 => 99
  }

  val aMappedList = (1 :: 2 :: 3 :: List.empty) map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  val manualPf = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2
    override def apply(v1: Int): Int = v1 match {
      case 1 => 2
      case 2 => 1
    }
  }

  object lowercase {
    def unapply(str : String) : Option[String] =
      Some(str.toLowerCase())
  }

  val chatbot : PartialFunction[String, String] = {
    case lowercase("hello") => "Hi, buddy!"
    case lowercase("goodbye") => "Bye, see you soon."
    case lowercase("what is your name?") => "Hi, my name is John. What is your name?"
    case x => s"Hi $x, nice to meet you."
  }
  scala.io.Source.stdin.getLines().foreach(line => println(s"John: ${chatbot(line)}"))
}
