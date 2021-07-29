package lectures.part4implicits

object PimpMyLibrary extends App {

  implicit class RichInt(value : Int) {
    def isEven : Boolean = value % 2 == 0
    def sqrt : Double = Math.sqrt(value)
    def times[T](f : => T) : IndexedSeq[T] = (1 to value).map(_ => f)
    def *[T](list : List[T]) : List[T] = (1 to value).flatMap(_ => list).toList
  }

  println(42.isEven)
  println(4.sqrt)


  implicit class RichString(string: String) {
    def asInt : Int = string
    val cipher : Map[Char, Char] = Map (
      'g' -> 'a',
      'a' -> 'g',
      'd' -> 'e',
      'e' -> 'd',
      'r' -> 'y',
      'y' -> 'r',
      'p' -> 'o',
      'o' -> 'p',
      'l' -> 'u',
      'u' -> 'l',
      'k' -> 'i',
      'i' -> 'k',
    )
    def encrypt : String = string.map(c => cipher.getOrElse(c, c))
  }

  println("jakub".encrypt)
  println("123".asInt)

  3.times {
    println("XD")
  }

  println(3 * List(1, 2))

  implicit def intOfString(string: String): Int = string.toInt
  println("4" / 2)
}
