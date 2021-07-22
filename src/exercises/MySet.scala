package exercises

import scala.language.postfixOps

trait MySet[A] extends (A => Boolean) {
  def contains(elem : A) : Boolean
  def +(elem : A) : MySet[A]
  def -(elem: A): MySet[A] = filter(_ != elem)
  def &(anotherSet : MySet[A]) : MySet[A] =
    filter(anotherSet)
  def --(anotherSet : MySet[A]) : MySet[A] =
    filter(!anotherSet)
  def ++(anotherSet: MySet[A]) : MySet[A]
  def apply(v1: A): Boolean = contains(v1)

  def map[B](f : A => B) : MySet[B]
  def flatMap[B](f : A => MySet[B]) : MySet[B]
  def filter(predicate: A => Boolean) : MySet[A]
  def foreach(f: A => Unit) : Unit
  def unary_! : MySet[A]
}

class ListSet[A](protected val elems : List[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elems.contains(elem)

  override def +(elem: A): MySet[A] =
    if (contains(elem))
      this
    else
      ListSet(elem :: elems)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    elems.foldLeft(anotherSet)((l, e) => l + e)

  override def map[B](f: A => B): MySet[B] =
    ListSet(elems.map(f))

  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    elems.foldLeft[MySet[B]](ListSet.empty)((s, a) => s ++ f(a))

  override def filter(predicate: A => Boolean): MySet[A] =
    ListSet(elems.filter(predicate))

  override def foreach(f: A => Unit): Unit =
    elems.foreach(f)

  override def unary_! : MySet[A] = PropertySet(x => !this(x))
}

class PropertySet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] =
    PropertySet(x => x == elem || property(x))

  override def ++(anotherSet: MySet[A]): MySet[A] =
    PropertySet(x => anotherSet(x) || property(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] =
    PropertySet(x => predicate(x) && property(x))

  override def foreach(f: A => Unit): Unit = politelyFail

  override def unary_! : MySet[A] = PropertySet(x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

object PropertySet {
  def apply[A](property : A => Boolean) = new PropertySet[A](property)
  def empty[A] : PropertySet[A] = PropertySet[A](_ => false)
}

object ListSet {
  def empty[A] : ListSet[A] = ListSet(List.empty[A])
  def apply[A](elems : A*) : ListSet[A] = new ListSet[A](elems.toList)
  def apply[A](elems : List[A]) : ListSet[A] = new ListSet[A](elems)
}

object MySetPlayground extends App {
  val s = ListSet(1, 2, 3, 4)
  s + 5 ++ ListSet(2, 8) + 4 map (_ * 10) filter (_ > 10) foreach println

  val neg = !s
  1 to 10 filter (neg filter (_ % 2 == 0)) foreach println
}
