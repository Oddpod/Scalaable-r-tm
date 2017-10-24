import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId
  def withdraw(amount: Double): Unit = this.synchronized {
    if(amount > getBalanceAmount) {
      throw new NoSufficientFundsException()
    }  else if (amount < 0) {
      throw new IllegalAmountException("Only positive numbers")
    } else {
      balance.amount -= amount;
    }
  }
  def deposit(amount: Double): Unit = this.synchronized {
    if(amount < 0) {
      throw new IllegalAmountException("It's a deposit, not a withdrawal");
    }
    balance.amount += amount;
  }
  def getBalanceAmount: Double = {
    balance.amount;
  }

  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
