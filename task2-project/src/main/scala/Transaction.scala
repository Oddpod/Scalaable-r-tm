import exceptions.{IllegalAmountException, NoSufficientFundsException}
import scala.collection.mutable
object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue extends mutable.SynchronizedQueue[Transaction] {

  // Remove and return the first element from the queue
  def pop: Transaction = dequeue()

  // Return whether the queue is empty
  // Add new element to the back of the queue
  def push(t: Transaction): Unit = enqueue(t)

  // Return the first element from the queue without removing it
  def peek: Transaction = front


}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempts = 1;
  override def run: Unit = {

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

    try {
      if (from.uid < to.uid) from synchronized {
        to synchronized {
          doTransaction
        }
      } else to synchronized {
        from synchronized {
          doTransaction
        }
      }
      status = TransactionStatus.SUCCESS
      processedTransactions += this;
      // Extend this method to satisfy new requirements.

    } catch
        { case  e: NoSufficientFundsException => {
          status = TransactionStatus.FAILED
          if(allowedAttemps > attempts) {
            attempts += 1;
            transactionsQueue += this;
          } else {
            processedTransactions += this;
          }
        } case e: IllegalAmountException => {
          status = TransactionStatus.FAILED
          if(allowedAttemps > attempts) {
            attempts += 1;
            transactionsQueue += this

          } else {
            processedTransactions += this;
          }
      }
    }
  }
}
