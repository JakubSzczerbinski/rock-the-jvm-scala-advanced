package lectures.part4implicits

object OrganizingImplicits extends App {
  implicit val reverseOrdering : Ordering[Int] = Ordering.fromLessThan(_ > _)
//  implicit val normalOrdering : Ordering[Int] = Ordering.fromLessThan(_ < _)
  println(List(3, 2, 1, 5, 4).sorted)

  case class Person(name : String, age : Int)
  object Person {
    implicit val alphabeticalOrdering: Ordering[Person] =
      Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }
  implicit val ageOrdering: Ordering[Person] =
    Ordering.fromLessThan((a, b) => a.age < b.age)

  val persons = List(
    Person("Guy", 30),
    Person("Andy", 21),
    Person("Steve", 15)
  )

  println(persons.sorted)

  case class Purchase(nUnits : Int, unitPrice : Int) {
    def totalPrice : Int = nUnits * unitPrice
  }
  object Purchase {
    implicit val totalPriceOrdering : Ordering[Purchase] =
      Ordering.by(_.totalPrice)
  }

  object UnitsOrdering {
    implicit val unitsOrdering : Ordering[Purchase] =
      Ordering.by(_.nUnits)
  }

  object PriceOrdering {
    implicit val priceOrdering : Ordering[Purchase] =
      Ordering.by(_.unitPrice)
  }

  val purchases = List(
    Purchase(10, 1),
    Purchase(2, 23),
    Purchase(3, 20)
  )

  println(purchases.sorted)

}
