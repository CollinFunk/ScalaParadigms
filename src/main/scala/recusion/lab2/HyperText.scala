package recusion.lab2
import scala.annotation.tailrec

class Base {
  def inc(x: BigInt): BigInt = x + 1
  def dec(x: BigInt): BigInt = x - 1
  def isZero(x: BigInt): Boolean = x == 0
}

class Hyper extends Base {

  def add(n: BigInt, m: BigInt): BigInt =
    if (m > 0)
      add(inc(n), m - 1)
    else if (m < 0)
      add(dec(n), m + 1)
    else
      n

  def mul(n: BigInt, m: BigInt): BigInt =
    if (isZero(n))
      0
    else
      m + mul(dec(n), m)

  def exp(n: BigInt): BigInt =
    if(isZero(n))
      1
    else
      mul(2,exp(dec(n)))

  //def hyperExp(n: BigInt): BigInt =

  // etc.
}

class TailHyper extends Base {

  @tailrec final def add(n: BigInt, m: BigInt): BigInt =
    if(isZero(n))
      m
    else
      add(dec(n), inc(m))

  def mul(n: BigInt, m: BigInt): BigInt =
    @tailrec def mulHelper(n: BigInt, m: BigInt, r: BigInt) : BigInt =
      if (isZero(n))
        r
      else
        mulHelper(dec(n), m, add(m,r))
    mulHelper(n, m ,0)


  def exp(n: BigInt): BigInt =
    @tailrec def expHelper(n: BigInt, m: BigInt) : BigInt =
      if (isZero(n))
        m
      else
        expHelper(dec(n), mul(2,m))
    expHelper(n, 1)

  //def hyperExp(n: BigInt): BigInt = ???

  // etc.
}


object HyperTest extends TailHyper with App  {
  println(add(4, 5))
  println(mul(7, 5))
  println(exp(10))


  // println("exp(10) = " + exp(10))           // 1024
  // println("hyperExp(2) = " + hyperExp(2))   // 16
  // println("hyperExp(3) = " + hyperExp(3))   // 65536
  // println("hyperExp(4) = " + hyperExp(4))   // still waiting

}
