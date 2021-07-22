package exercises

abstract class MyStream[+A] {
  def isEmpty : Boolean
  def head: A
  def tail: MyStream[A]
  def #::[B >: A](element: B): MyStream[B] = new ConsStream[B](element, this)
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] =
    if (isEmpty)
      anotherStream
    else
      new ConsStream[B](head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def map[B](f: A => B): MyStream[B] =
    new ConsStream[B](f(head), tail.map(f))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head))
      new ConsStream[A](head, tail.filter(predicate))
    else
      tail.filter(predicate)

  def take(n: Int): MyStream[A] =
    n match {
      case _ if n <= 0 => new EmptyStream[A]()
      case 1 => new ConsStream[A](head, new EmptyStream[A]())
      case _ => new ConsStream[A](head, tail.take(n - 1))
    }

  def takeAsList(n: Int): List[A] = {
    n match {
      case _ if n <= 0 => List()
      case 1 => List(head)
      case _ => head :: tail.takeAsList(n - 1)
    }
  }
}

class EmptyStream[+A] extends MyStream[A] {
  override def isEmpty: Boolean = true

  override def head: A = ???

  override def tail: MyStream[A] = ???

  override def foreach(f: A => Unit): Unit = ()

  override def map[B](f: A => B): MyStream[B] = new EmptyStream[B]

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = new EmptyStream[B]

  override def filter(predicate: A => Boolean): MyStream[A] = this

  override def take(n: Int): MyStream[A] = {
    assert(n == 0)
    new EmptyStream[A]
  }

  override def takeAsList(n: Int): List[A] = {
    assert(n == 0)
    List.empty
  }
}

class ConsStream[+A](val head: A, t: => MyStream[A]) extends MyStream[A] {
  override lazy val tail : MyStream[A] = t

  override def isEmpty: Boolean = false
}


object MyStream {
  def from[A](start: A)(generator: A => A) : MyStream[A] = new ConsStream[A](start, from(generator(start))(generator))
}

object StreamPlayground extends App {
  val natural_numbers = MyStream.from(1)(x => {println(s"call=$x -> ${x + 1}"); x + 1})
  (natural_numbers.take(5) ++ natural_numbers.filter(_ % 2 == 0).map(x => x * x)).take(10).foreach(println)
  natural_numbers.flatMap(x => MyStream.from(x)(x => x * x).take(3)).take(25).foreach(println)

  def fib(s1 : Int, s2 : Int) : MyStream[Int] =
    new ConsStream[Int](s1, fib(s2, s1 + s2))

  def primes(ns : MyStream[Int]) : MyStream[Int] = {
    if (ns.isEmpty)
      ns
    else
      new ConsStream[Int](ns.head, primes(ns.tail.filter(_ % ns.head != 0)))
  }

  val fib_stream = fib(1, 1)
  fib_stream.take(10).foreach(println)
  val prime_stream = primes(MyStream.from(2)(_ + 1))
  prime_stream.take(10).foreach(println)
}


