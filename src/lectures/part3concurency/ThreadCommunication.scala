package lectures.part3concurency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {

  class SimpleContainer {
    private var value : Int = 0;
    def isEmpty : Boolean = value == 0
    def set(newValue : Int) = value = newValue
    def get : Int = {
      val result = value
      value = 0
      result
    }
  }
  def prodCons() : Unit = {
    val container = new SimpleContainer
    val consumer = new Thread(() => {
      println("[Consumer] is waiting.")
      container.synchronized {
        while (container.isEmpty) {
          container.wait()
        }
        println(s"[Consumer] got value ${container.get}")
      }
    })

    val producer = new Thread(() => {
      println("[Producer] computing.");
      Thread.sleep(100);
      val value = 42;
      println(s"[Producer] I have produced a value $value")
      container.synchronized {
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }
//  prodCons()

  def prodConsLargeBuffer() : Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]()
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random();
      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty)
            buffer.wait()

          val x = buffer.dequeue();
          println(s"[Consumer] $x")
          buffer.notify()
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while (true) {
        buffer.synchronized {
          if (buffer.size == capacity)
            buffer.wait()

          buffer.enqueue(i)
          buffer.notify()
        }
        i += 1
        Thread.sleep(random.nextInt(500))
      }
    })

    producer.start()
    consumer.start()
  }

  def multProdCons() : Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]()
    val capacity = 15

    def makeConsumer(id : Int) : Thread = new Thread(() => {
      val random = new Random();
      while (true) {
        buffer.synchronized {
          while (buffer.isEmpty)
            buffer.wait()

          val x = buffer.dequeue();
          println(s"[Consumer #$id] $x")
          buffer.notify()
        }
        Thread.sleep(random.nextInt(250))
      }
    })

    def makeProducer(id : Int) : Thread = new Thread(() => {
      val random = new Random()
      var i = 0
      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity)
            buffer.wait()

          println(s"[Producer #$id] producing $i")
          buffer.enqueue(i)
          buffer.notify()
        }
        i += 1
        Thread.sleep(random.nextInt(500))
      }
    })

    (1 to 6).map(makeProducer).foreach(_.start())
    (1 to 3).map(makeConsumer).foreach(_.start())
  }

  multProdCons()
}
