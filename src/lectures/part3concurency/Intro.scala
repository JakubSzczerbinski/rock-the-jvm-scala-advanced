package lectures.part3concurency

import java.util.concurrent.Executors

object Intro extends App {
  val aThread = new Thread(() => {
    println("Running in parallel.")
  })
  aThread.start()

  val threadHello = new Thread(() => (1 to 5) foreach (x => println(s"Hello ${x}")))
  val threadGoodbye = new Thread(() => (1 to 5) foreach (x => println(s"Goodbye ${x}")))
  threadHello.start()
  threadGoodbye.start()

  val pool = Executors.newFixedThreadPool(10);
  pool.execute(() => println("From pool"));

  pool.execute(() => {
    Thread.sleep(1000)
    println("Done after 1 second");
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("Done after 2 seconds")
  })

  def inceptionThread(n : Int): Thread = {
    val runnable : Runnable =
      if (n <= 0)
        () => println("Most inner thread")
      else {
        val thread = inceptionThread(n-1)
        () => println(s"Thread $n"); thread.run()
      }
    new Thread(runnable)
  }

  val inception50 = inceptionThread(50);
  inception50.start()

}
