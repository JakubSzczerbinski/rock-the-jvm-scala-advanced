package lectures.part4implicits

import java.util.Date

object JSONSerialization extends App {
  case class User(name : String, age : Int, email : String)
  case class Post(content: String, createdAt : Date)
  case class Feed(user : User, posts: List[Post])

  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String =
      values.map({ case (name, value) => s"\"$name\": ${value.stringify}" })
            .mkString("{", ", ", "}")
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String =

        values.map(_.stringify).mkString("[", ", ", "]")
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Daniel"),
    "posts" -> JSONArray(List(
      JSONString("XD")
    ))
  ))
  println(data.stringify)

  trait JSONConverter[T] {
    def convert(value : T) : JSONValue
  }

  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }

  implicit object UserConverter extends JSONConverter[User] {
    override def convert(user: User): JSONValue =
      JSONObject(Map(
        "name" -> JSONString(user.name),
        "age" -> JSONNumber(user.age),
        "email" -> JSONString(user.email)
      ))
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(post: Post): JSONValue =
      JSONObject(Map(
        "content" -> JSONString(post.content),
        "createdAt" -> JSONString(post.createdAt.toString)
      ))
  }

  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(feed: Feed): JSONValue =
      JSONObject(Map(
        "user" -> feed.user.toJSON,
        "posts" -> JSONArray(feed.posts.map(_.toJSON))
      ))
  }

  implicit class JSONOps[T](value : T) {
    def toJSON(implicit converter: JSONConverter[T]) : JSONValue =
      converter.convert(value)
  }

  val now = new Date()
  val jakub = User("Jakub", 23, "jakub@szczerbin.ski")
  val feed = Feed(jakub, List(
    Post("12", now),
    Post("Another post", now)
  ))
  println(feed.toJSON.stringify)
}
