import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
  var balance : Double = initialBalance
  def withdraw(amount: Double): Unit = this.synchronized {
    if(amount > getBalanceAmount) {
      throw new NoSufficientFundsException()
    }  else if (amount < 0) {
      throw new IllegalAmountException("Only positive numbers")
    } else {
     balance -= amount
    }
  }
  def deposit(amount: Double): Unit = this.synchronized {
    if(amount < 0) {
      throw new IllegalAmountException("It's a deposit, not a withdrawal");
    }
    balance += amount
  }
  def getBalanceAmount: Double = {
    balance
  }
}
