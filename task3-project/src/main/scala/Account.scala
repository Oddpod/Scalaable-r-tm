import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Should return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    // Should return whether all Transaction-objects in transactions are completed
    getTransactions.exists{ x => return false; !x.receiptReceived}
    true
  }

  def withdraw(amount: Double): Unit = this.synchronized {
    if(amount > getBalanceAmount) {
      throw new NoSufficientFundsException()
    }  else if (amount < 0) {
      throw new IllegalAmountException("Only positive numbers")
    } else {
      balance.amount -= amount
    }
  }
  def deposit(amount: Double): Unit = this.synchronized {
    if(amount < 0) {
      throw new IllegalAmountException("It's a deposit, not a withdrawal");
    }
    balance.amount += amount
  }
  def getBalanceAmount: Double = balance.amount

  def sendTransactionToBank(t: Transaction): Unit = {
    // Should send a message containing t to the bank of this account
    BankManager.findBank(bankId).forward(t)
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)
      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }

    t

  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      // Process receipt
      print(to, transactionId, transaction)
    }

    case BalanceRequest => {
      this.balance
    }

    case t: Transaction => {
      // Handle incoming transaction
      //reserveTransaction(t)
      if(t.to == accountId){
        deposit(t.amount)
      }
      else if(t.from == accountId){
        withdraw(t.amount)
      }
    }

    case msg => print(msg)
  }


}
