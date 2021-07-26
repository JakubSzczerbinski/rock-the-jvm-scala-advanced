package lectures.part3concurency

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.runtime.java8.JFunction0$mcC$sp
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

object FuturePromises extends App {
  def calculateMeaningOfLife : Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife
  }

  aFuture.onComplete {
    case Success(value) => println(s"Meaning of life: $value")
    case Failure(exception) => println(s"Exception $exception")
  }

  case class Profile(id : String, name : String) {
    def poke(anotherProfile : Profile) : Unit = {
      println(s"${this.name} poking ${anotherProfile.name}")
    }
  }

  object SocialNetwork {
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    def fetchProfile(id : String) : Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile) : Future[Profile] = Future {
      Thread.sleep(random.nextInt(500))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  mark.onComplete {
    case Success(markProfile) =>
      SocialNetwork.fetchBestFriend(markProfile).onComplete {
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(e) => e.printStackTrace()
      }
    case Failure(e) => e.printStackTrace()
  }

  val x : Future[Unit] = for {
    markProfile <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    billProfile <- SocialNetwork.fetchBestFriend(markProfile)
  } yield markProfile.poke(billProfile)

  val aProfile = SocialNetwork.fetchProfile("fake id").recover {
    case e : Throwable => Profile("fb.id.0-dummy", "Dummy")
  }

  val otherProfile = SocialNetwork.fetchProfile("fake id").recoverWith {
    case e : Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackProfile = SocialNetwork.fetchProfile("fake id")
    .fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  case class User(name : String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"
    def fetchUser(name : String) : Future[User] = Future {
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user : User, merchantName : String, amount : Double): Future[Transaction] = Future {
      Thread.sleep(500)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username : String, item : String, merchantName : String, cost : Double): String = {
      val transactionStatus = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status
      Await.result(transactionStatus, 2.seconds)
    }
  }

  println(BankingApp.purchase("Jakub", "Xiaomi Redmi 8", "X-KOM.pl", 8000))

  val promise = Promise[Int]()
  val future = promise.future

  future.onComplete {
    case Success(r) => println("[consumer] i've received " + r)
  }

  val producer = new Thread(() => {
    println("[producer] crunching numbers")
    Thread.sleep(500)
    promise.success(42)
    println("[producer] done")
  })
  producer.start()

  def immediateFuture[T](value : T) : Future[T] = Future(value)

  def inSequence[A, B](fa : Future[A], fb : Future[B]) : B = {
    Await.result(fa.flatMap(_ => fb), 2.seconds)
  }

  println(inSequence(
    Future{ Thread.sleep(100); println("A"); 42},
    Future{ println("B"); 32}
  ))

  def first[T](fa : Future[T], fb : Future[T]) = {
    val promise = Promise[T]()
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)
    Await.result(promise.future, 2.seconds)
  }

  println(first(
    Future{ println("B"); 32},
    Future{ Thread.sleep(100); println("A"); 42 }
  ))

  def last[T](fa : Future[T], fb : Future[T]) = {
    val promiseFirst = Promise[T]()
    val promiseSecond = Promise[T]()
    val handler : Try[T] => Unit = x =>
      if (!promiseFirst.tryComplete(x))
        promiseSecond.complete(x)
    fa.onComplete(handler)
    fb.onComplete(handler)
    Await.result(promiseSecond.future, 2.seconds)
  }

  println(last(
    Future{ println("B"); 32},
    Future{ Thread.sleep(100); println("A"); 42 }
  ))

  def retryUntil[T](action : () => Future[T], cond : T => Boolean) : T = {
    def aux() : Future[T] = {
      action().filter(cond).recoverWith(_ => aux())
    }
    Await.result(aux(), 2.seconds)
  }
  val random = new Random()
  val action = () => Future { random.nextInt(10) }
  val cond : Int => Boolean = x => x < 2
  println(retryUntil(action, cond))
  Thread.sleep(3000)
}
