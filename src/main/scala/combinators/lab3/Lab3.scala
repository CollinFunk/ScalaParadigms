package combinators.lab3

class Lab3 {
  // problem 1
  def compose[T](f: T=>T, g: T=>T): T => T = (t:T) => f(g(t))

  // problem 2
  def selfIter[T](f: T=>T, n: Int): T => T =
    if(n == 1)
      f
    else
      compose(f, selfIter(f, n - 1))

  // problem 3
  def countPass[T](elems: Array[T], test: T => Boolean): Int =
    if (elems.isEmpty)
      0
    else if(test(elems.head))
      1 + countPass(elems.drop(1), test)
    else
      countPass(elems.drop(1), test)

  // problem 5
  def makeIter(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int =
    def helper(n: Int): Int =
      if (n == 0)
        baseVal
      else
        combiner(n, helper(n - 1))
    helper


  // problem 6
  def deOptionize[T, S](f: T => Option[S]): T => S =
    def helper(parameter: T): S = {
      f(parameter) match {
        case None => throw new Exception("Option returning function returned None")
        case Some(x) => x
      }
    }
    helper
}
