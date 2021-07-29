package lectures.part4implicits

object TypeClasses extends App {

  trait HTMLWritable {
    def toHtml : String
  }

  case class User(name : String, age : Int, email : String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name($age) <a href=$email/> </div>"
  }

  val jakub = User("Jakub", 25, "jakub(at)szczerbin.ski")
  println(jakub.toHtml)

  trait HtmlSerializer[T] {
    def serialize(value : T) : String
  }

  implicit object UserSerializer extends HtmlSerializer[User] {
    override def serialize(value: User): String = value.toHtml
  }

  import java.util.Date
  object DateSerializer extends HtmlSerializer[Date] {
    override def serialize(value: Date): String = s"<div> ${value.toString} </div>"
  }

  object PartialUserSerializer extends HtmlSerializer[User] {
    override def serialize(value: User): String = s"<div> ${value.name} </div>"
  }

  println(UserSerializer.serialize(jakub))
  println(PartialUserSerializer.serialize(jakub))
  println(DateSerializer.serialize(new Date()))

  object HtmlSerializer {
    def serialize[T](value : T)(implicit serializer : HtmlSerializer[T]) : String =
      serializer.serialize(value)
  }

  implicit object IntSerializer extends HtmlSerializer[Int] {
    override def serialize(value: Int): String = s"<span> $value </span>"
  }
  println(HtmlSerializer.serialize(1))
  println(HtmlSerializer.serialize(jakub))

  implicit class HTMLEnrichment[T](value : T) {
    def toHTML(implicit serializer: HtmlSerializer[T]) : String = serializer.serialize(value)
  }

  println(2.toHTML)
  println(jakub.toHTML)
  println(jakub.toHTML(PartialUserSerializer))

  def htmlBoilerplate[T](content : T)(implicit serializer : HtmlSerializer[T]) : String =
    s"<html><body> ${content.toHTML(serializer)} </body></html>"

  def htmlSugar[T : HtmlSerializer](content : T) = {
    val serializer = implicitly[HtmlSerializer[T]]
    s"<html><body> ${content.toHTML(serializer)} </body></html>"
  }

  case class Permissions(mask : String)
  implicit val defaultPermissions : Permissions = Permissions("0744")

  val standardPerms = implicitly[Permissions]
  val standardPermsDIY = implicitlyDIY[Permissions]

  def implicitlyDIY[T](implicit t : T) = t
}
