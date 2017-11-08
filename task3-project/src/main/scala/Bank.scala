import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1)
    val accountIdString = accountCounter.incrementAndGet().toString
    BankManager.createAccount(accountIdString, bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    Option(BankManager.findAccount(bankId, accountId))
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    Option(BankManager.findBank(bankId))
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance)// Create a new account
    case GetAccountRequest(id) => findAccount(id) // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)

    case t: TransactionRequestReceipt => {
      // Forward receipt
      BankManager.findBank(bankId).forward(t)
    }

    case msg => print("this is a message: " + msg)
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout = new Timeout(5 seconds)
    val isInternal = t.to.length <= 4
    val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
    val toAccountId = if (isInternal) t.to else t.to.substring(4)
    val transactionStatus = t.status

    try {
      if(isInternal) {
      findAccount(toAccountId).get match {
        case a: ActorRef => a ! t
      }} else {
        findOtherBank(toBankId).get match {
          case a: ActorRef => a ! t
        }
      }

      }catch { case _ => None
      t.status = TransactionStatus.FAILED
      sender ! TransactionRequestReceipt(toAccountId, t.id, t)
    }
  }
}