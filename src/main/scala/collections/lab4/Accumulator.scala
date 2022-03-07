package collections.lab4



object Accumulator extends App {
  def inc(r: Int): Int = r + 1
  def add(n: Int): Int => Int =
    _+n

  def mul(n: Int): Int => Int =
    _ * n

  def clear(n: Int): Int => Int =
    _ * 0

  def display(n: Int): Int => Int =
    println(_)
    _ + 0


  def accum(program: List[Int => Int]): Int =
    var reg: Int = 0
    for (i <- 0 to program.length - 1)
      reg = program(i).apply(reg)
      println(reg)
    1

  val program =
    List(add(3), mul(4), add(5), add(9), mul(2))

  accum(program)
}