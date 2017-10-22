import org.scalatest.FunSuite
import exceptions._

class AccountTests extends FunSuite {

	test("Test: Valid account withdrawal"){
		val acc = new Account(500)
	  acc.withdraw(250)
		assert (acc.getBalanceAmount == 250)
	}
	
	test("Test: Invalid account withdrawal should throw exception"){
		val acc = new Account(500)
		intercept[NoSufficientFundsException] {
		  acc.withdraw(750)
		}
	}
	
	test("Test: Withdrawal of negative amount should throw exception"){
		val acc = new Account(500)
		intercept[IllegalAmountException] {
		  acc.withdraw(-100)
		}
	}
	
	test("Test: Valid account deposit"){
		val acc = new Account(500)
		acc.deposit(250)
		assert (acc.getBalanceAmount == 750)
	}
	
	test("Test: Deposit of negative amount should throw exception"){
		val acc = new Account(500)
		intercept[IllegalAmountException] {
		  acc.deposit(-50)
		}
	}
	
	test("Test: Correct balance amount after several withdrawals and deposits") {
	  val acc = new Account(50000)
    val first = Main.thread {
	    for (i<- 0 until 100) { acc.withdraw(10); Thread.sleep(10) }
	    }
    val second = Main.thread {
      for (i<- 0 until 100) { acc.deposit(5); Thread.sleep(20) }
      }
    val third = Main.thread {
	    for (i<- 0 until 100) { acc.withdraw(50); Thread.sleep(10) }
	    }
    val fourth = Main.thread {
	    for (i<- 0 until 100) { acc.deposit(100); Thread.sleep(10) }
	    }
    first.join(); second.join(); third.join(); fourth.join()
    assert (acc.getBalanceAmount == 54500)
	}

}

class AccountTransferTests extends FunSuite {
  
  test("Test: Valid transfer between accounts") {
    val acc1 = new Account(100)
    val acc2 = new Account(200)
    Bank transaction (acc1, acc2, 50)
    assert ((acc1.getBalanceAmount == 50) && (acc2.getBalanceAmount == 250))
  }
  
  test("Test: Transfer of negative amount between accounts should throw exception") {
    val acc1 = new Account(500)
    val acc2 = new Account(1000)
    intercept[IllegalAmountException] {
      Bank transaction (acc1, acc2, -100)
    }
  }
  
  test("Test: Invalid transfer between accounts due to insufficient funds should throw exception") {
    val acc1 = new Account(100)
    val acc2 = new Account(1000)
    intercept[NoSufficientFundsException] {
      Bank transaction (acc1, acc2, 150)
    }
  }
  
  test("Test: Correct balance amounts after several transfers") {
    val acc1 = new Account(3000)
    val acc2 = new Account(5000)
    val first = Main.thread {
      for (i<- 0 until 100) { Bank transaction (acc1, acc2, 30) }
      }
    val second = Main.thread {
      for (i<- 0 until 100) { Bank transaction (acc2, acc1, 23) }
      }
    first.join(); second.join()
    assert ((acc1.getBalanceAmount == 2300) && (acc2.getBalanceAmount == 5700))
  }
  
  
}
