package collections.lab4

import scala.annotation.tailrec

class Transaction(val amt: Double, fromAcct: Int, toAcct: Int) {
  def getFromAcc(): Int =
    fromAcct

  def getToAcc(): Int =
    toAcct

  def getAmt(): Double =
    amt
}

class balanceCalc {

  def balance(acct: Int, ledger: List[Transaction]): Double = {
    @tailrec
    def helper(acct: Int, ledger: List[Transaction], sum: Double): Double =
      if (ledger.isEmpty)
        sum
      else if (ledger.head.getFromAcc() != acct && ledger.head.getToAcc() != acct)
        helper(acct, ledger.drop(1), sum)
      else if (ledger.head.getFromAcc() == acct)
        helper(acct, ledger.drop(1), sum - ledger.head.getAmt())
      else
        helper(acct, ledger.drop(1), sum + ledger.head.getAmt())
    helper(acct, ledger, 0)
  }
}


