import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {
  private val counter: AtomicInteger = new AtomicInteger()
  private val uid = generateAccountId;
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = Executors.newFixedThreadPool(allowedAttempts);

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    executorContext.submit(new Runnable {override def run(): Unit = { processTransactions}})
  }

  def generateAccountId: Int = {
    counter.incrementAndGet()
  }

  private def processTransactions: Unit = {
    val transaction = transactionsQueue.pop
    executorContext.submit(transaction)
    Thread.sleep(30);
    executorContext.submit(new Runnable {override def run(): Unit = {processTransactions}})
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    println(processedTransactions.size)
    processedTransactions.iterator.toList
  }

}
