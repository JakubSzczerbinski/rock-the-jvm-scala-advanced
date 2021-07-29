package lectures.part4implicits

object EqualityPlayground extends App {
  case class User(name : String, age : Int, email : String) {
    def toHtml: String = s"<div>$name($age) <a href=$email/> </div>"
  }

  val jakub = User("Jakub", 25, "jakub(at)szczerbin.ski")

  trait Equal[T] {
    def equal(lhs : T, rhs : T) : Boolean
  }

  object Equal {
    def apply[T](a : T, b : T)(implicit equal: Equal[T]): Boolean = equal.equal(a, b)
  }

  implicit object NameEqual extends Equal[User] {
    override def equal(lhs: User, rhs: User): Boolean = lhs.name == rhs.name
  }

  object NameEmailEqual extends Equal[User] {
    override def equal(lhs: User, rhs: User): Boolean =
      NameEqual.equal(lhs, rhs) && lhs.email == rhs.email
  }

  implicit class EqEnrichment[T](rhs : T) {
    def ===(lhs : T)(implicit eq : Equal[T]): Boolean = eq.equal(lhs, rhs)
    def !==(lhs : T)(implicit eq : Equal[T]): Boolean = !eq.equal(lhs, rhs)
  }

  println(Equal(jakub, jakub))
  println(jakub === jakub)
  println(jakub !== jakub)
}
