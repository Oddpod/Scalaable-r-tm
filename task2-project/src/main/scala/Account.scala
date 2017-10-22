import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = ??? // Like in part 1
  def deposit(amount: Double): Unit = ??? // Like in part 1
  def getBalanceAmount: Double = ??? // Like in part 1

  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
