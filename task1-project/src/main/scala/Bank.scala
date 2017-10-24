object Bank {

  private var idCounter: Int = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = ??? // Implement

  def getUniqueId: Int = {
    idCounter += 1 // Can this be improved?
    idCounter
  }

}
