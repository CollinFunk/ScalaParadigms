package collections.lab4

trait instruction {
  def impl(reg: Int): Int
}


object Accumulator {
  var register: Int = 0
  def accum(program: List[instruction]): Int =
    program.foreach(command => command.impl())
    register
}

object accuTest extends Accumulator extends App {
  val program =
    List(add(3), mul(4), add(5), display _, clear _, add(9), mul(2))

  Accumulator.accum(program)
}