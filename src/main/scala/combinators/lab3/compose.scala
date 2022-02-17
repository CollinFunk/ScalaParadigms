package combinators.lab3


class compose {
  // problem 1
  def compose[T](f: T=>T, g: T=>T): T => T = (t:T) => f(g(t))

  // problem 2
  def selfIter[T](f: T=>T, n: Int): T => T =
    if(n == 1)
      f
    else
      compose(f, selfIter(f, n - 1))

  def inc(x: Double) = x + 1
  def double(x: Double) = 2 * x

  // problem 3, to do
  //def countPass[T](arr: Array[T], test: T => Boolean): Int =

  //problem 4
  def makeRecur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int =
    def recurHelper(n: Int): Int =
      if (n == 0)
        baseVal
      else
        combiner(n, recurHelper(n - 1))
    recurHelper

  def adder(n: Int, m: Int): Int = n + m
  def multiplier(n: Int, m: Int): Int = n * m

  def tri(n: Int): Int  =
    if (n == 0)
      0
    else
      n + tri(n - 1)

  def fact(n: Int): Int =
    if (n == 0)
      1
    else
      n * fact(n - 1)

}

object composeTest extends compose with App {
  println(selfIter(inc, 5)(10))
  println(makeRecur(0, adder)(6))
  println(tri(6))
  println(makeRecur(1, multiplier)(7))
  println(fact(7))
}
