object Main extends App {

  def thread(body: => Unit): Thread = {
      val t = new Thread {
        override def run() = body
      }
      t.start
      t
    }

  // Write a few transaction examples using Threads

}
