package basic.lab1

class NegativeInputException extends Exception("Inputs must be positive")
class OddInputException extends Exception("Inputs must be even")

object numerology extends App {
  // problem 1
  def kingdom(n: Int): Int =
    if (n % 2 == 0)
      if (10 < n)
        if (n % 100 == 0)
          2
        else
          1
      else
        3
    else
      4

  // testing
  println(s"kingdom(5) = ${kingdom(5)}")     // 4
  println(s"kingdom(11) = ${kingdom(11)}")   // 4
  println(s"kingdom(16) = ${kingdom(16)}")   // 1
  println(s"kingdom(5) = ${kingdom(5)}")     // 4
  println(s"kingdom(200) = ${kingdom(200)}") // 2

  // problem 2
  def order(n: Int): Int =
    if (n < 0)
      0
    else
      family(n) * ilk(n) + genus(n)

  def family(n: Int) =
    if (n % 3 == 0)
      1
    else
      2

  def ilk(n: Int) =
    if (n == 50)
      3
    else
      4

  def genus(n: Int) =
    if (n % 7 == 0)
      5
    else
      6

  println(s"order(-1) = ${order(-1)}")   // 0
  println(s"order(15) = ${order(15)}")   // 10
  println(s"order(50) = ${order(50)}")   // 12
  println(s"order(49) = ${order(49)}")   // 13
  println(s"order(21) = ${order(21)}")   // 9

  // problem 3

  // original version
  def species(n: Int) =
    if (0 < n) if (n % 2 == 0) 1 else 2

  // corrected version
  def species2(n: Int): Int =
    if (n > 0)
      if (n % 2 == 0)
        1
      else
        2
    else
      2

  println(s"species(1) = ${species(1)}")
  println(s"species(-1) = ${species(-1)}")
  println(s"species(4) = ${species(4)}")
  println(s"species2(1) = ${species2(1)}")
  println(s"species2(-1) = ${species2(-1)}")
  println(s"species2(4) = ${species2(4)}")



  // problem 4

  // odd positives are realm 1
  def realm1(n: Int): Int =
    if (n < 0) throw NegativeInputException()
    if (n % 0 != 0) throw OddInputException()
    1


  // even positives not divisible by 3 are realm 2
 // def realm2(n: Int): Int =

  // even positives divisible by 6 and 7 are realm 3
 // def realm3(n: Int): Int =

  //def realm(n: Int): Int =

  //println(s"realm(0) = ${realm(0)}")   // 0
  //println(s"realm(5) = ${realm(4)}")   // 2
  //println(s"realm(42) = ${realm(42)}") // 3
  //println(s"realm(9) = ${realm(9)}")   // 1

}
