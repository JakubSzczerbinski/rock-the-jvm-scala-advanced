package lectures.part2afp

object CurriesPAF extends App {
  // curried functions
  val superAdder : Int => Int => Int = x => y => x + y
  val add3 = superAdder(3)
  println(add3(5))
  println(superAdder(3)(5))

  def curriedAdder(x: Int)(y: Int) : Int = x + y

  val add4 = curriedAdder(4) _
  println(add4(5))
  println(curriedAdder(4)(5))

  val simpleAddFunction = (x : Int, y : Int) => x + y
  def simpleAddMethod(x : Int, y : Int) = x + y
  def curriedAddMethod(x : Int)(y : Int) = x + y

  val add7_0 : Int => Int = y => simpleAddFunction(7, y)
  def add7_1(y : Int) = simpleAddMethod(7, y)
  def add7_2(y : Int) = curriedAdder(7)(y)
  val add7_3 = curriedAdder(7)(_)
  val add7_4 : Int => Int = curriedAdder(7)
  val add7_5 = simpleAddFunction(7, _ : Int)
  val add7_6 = simpleAddMethod(7, _)
  val add7_7 = simpleAddFunction.curried(7)

  def concatenator(a : String, b : String, c : String) = a + b + c
  val insertName = concatenator("Hello I'm ", _ : String, ", how are you?")
  println(insertName("Jakub"))

  val fillInBlanks = concatenator("Hello, ", _ : String, _ : String)
  println(fillInBlanks(" Daniel", " scala is awesome!"))

  def curriedFormatter(s : String)(number: Double) : String = s.format(number)

  def printNumbers(numbers : Float*) : Unit =
    printNumbers(numbers.toList)

  def printNumbers(numbers : List[Float]): Unit = {
    val formats = List("%4.2f", "%8.6f", "%14.12f")
    val format_fs = formats map (fmt => curriedFormatter(fmt)(_))
    numbers map (x => format_fs map (f => f(x)) mkString " ") foreach println
  }

  printNumbers(1.3213f, 3.854381f, 22.213f)

  def byName(n : => Int) = n + 1
  def byFunction(f : () => Int) = f() + 1

  def method: Int = 42
  def parenMethod() : Int = 42

  val f = () => () => 42
  def f_()() = 42
  byName(42)
  byName(method)
  byName(parenMethod) // <=> byName(parenMethod())
//  byName(() => 42)
//  byName(f())
//  byName(f_())

//  byFunction(42)
//  byFunction(method)
  byFunction(parenMethod)
  byFunction(() => 42)
  byFunction(f())
  byFunction(f_())
}
