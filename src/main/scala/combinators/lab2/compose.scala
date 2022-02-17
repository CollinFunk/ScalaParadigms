package combinators.lab2


object compose extends App {
  def compose[T](f: T=>T, g: T=>T): T => T = (t:T) => f(g(t))

  def selfIter[T](f: T=>T, n: Int): T => T = {
    if(n == 1)
      f
    else
      compose(f, selfIter(f, n - 1))
  }

  def inc(x: Double) = x + 1
  def double(x: Double) = 2 * x

  println(selfIter(inc, 5)(10))
}