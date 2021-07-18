package exercises

trait MySet[A] extends (A => Boolean) {
  def contains(elem : A) : Boolean
  def +(elem : A) : MySet[A]
  def ++(anotherSet: MySet[A]) : MySet[A]

  def map[B](f : A => B) : MySet[B]
  def flatMap[B](f : A => MySet[B]) : MySet[B]
  def filter(predicate: A => Boolean) : MySet[A]
  def fold[B](z : B)(f: (B, A) => B) : B
  def foreach(f: A => Unit) : Unit
}

class ListSet[A](protected val elems : List[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elems.contains(elem)

  override def +(elem: A): MySet[A] =
    if (contains(elem))
      this
    else
      ListSet(elem :: elems)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    anotherSet.fold[MySet[A]](this)((l, e) => l + e)

  override def map[B](f: A => B): MySet[B] =
    ListSet(elems.map(f))

  override def fold[B](z : B)(f: (B, A) => B) : B =
    elems.foldLeft(z)(f)

  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    elems.foldLeft[MySet[B]](ListSet.empty)((s, a) => s ++ f(a))

  override def filter(predicate: A => Boolean): MySet[A] =
    ListSet(elems.filter(predicate))

  override def foreach(f: A => Unit): Unit =
    elems.foreach(f)

  override def apply(v1: A): Boolean = contains(v1)
}

object ListSet {
  def empty[A] : ListSet[A] = ListSet(List.empty[A])
  def apply[A](elems : List[A]) = new ListSet[A](elems)
}
