package collections.lab4

class LazyLists {
  def allOnes(): LazyList[Int] =
    1 #:: allOnes()

  def nonNegative(): LazyList[Int] =
    def helper(n: Int): LazyList[Int] =
      n #:: helper(n+ 1)
    helper(0)

  def even(): LazyList[Int] =
    def helper(n: Int): LazyList[Int] =
      n #:: helper(n + 2)
    helper(0)

  def square(): LazyList[Int] =
    nonNegative().map(x => x * x)

}
