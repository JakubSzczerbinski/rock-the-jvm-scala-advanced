package lectures.part2afp

object Monads extends App {
  trait Attempt[+A] {
    def flatMap[B](f : A => Attempt[B]) : Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A) : Attempt[A] =
      try {
        Success(a)
      } catch {
        case e : Throwable => Fail(e)
      }
  }

  case class Success[A](value : A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }
  case class Fail(e : Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  val attempt = Attempt {
    throw new RuntimeException("XDDD")
  }

  println(attempt)

  class Lazy[+A](v : => A) {
    lazy val value : A = v
    def flatMap[B](f: (=> A) => Lazy[B]) : Lazy[B] =
      f(value)
  }

  object Lazy {
    def apply[A](v : => A) = new Lazy[A](v)
  }

  val lazyValue = Lazy {
    println(2)
    2
  } flatMap (x => Lazy {
    println(x + 1)
    x + 1
  })

  lazyValue.value

  class Monad[T] (t : T) {
    def flatMap[B](f: T => Monad[B]) : Monad[B] = ???
    def map[B](f : T => B) : Monad[B] = flatMap(x => Monad(f(x)))
    def flatten(m : Monad[Monad[T]]) : Monad[T] = m.flatMap(x => x)
  }

  object Monad {
    def apply[T](t : T): Monad[T] = new Monad[T](t)
  }

}
