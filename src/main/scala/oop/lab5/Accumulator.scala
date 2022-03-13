package oop.lab5

trait Instruction {
  def execute(reg: Double): Double
}

class Add(val arg: Double) extends Instruction {
  def execute(reg: Double): Double = reg + arg
}

class Mul(val arg: Double) extends Instruction {
  def execute(reg: Double): Double = reg * arg
}

class Halt extends Instruction {
  def execute(register: Double): Double = register
}

class Rep(val num: Int, val loopInstruction: Instruction) extends Instruction {
  def execute(register: Double): Double =
    var temp: Double = register
    for (i <- 1 to num)
      temp = loopInstruction.execute(temp)
    temp
}

class Blt(val numCheck: Int, val skipAmt: Int) extends Instruction {
  def execute(register: Double): Double =
    if (numCheck > register)
      skipAmt
    else
      0
}

object Accumulator {
  var register: Double = 0
  var skipCount: Double = 0
  var program: List[Instruction] = List[Instruction]()
  def run(): Unit =
    register = 0
    var haltCon: Boolean = false
    for (instruct <- program if haltCon.equals(false))
       if (instruct.isInstanceOf[Halt])
         haltCon = true
       else if (instruct.isInstanceOf[Blt])
         if (instruct.execute(register) != 0)
           skipCount = instruct.execute(register)
       if (skipCount > 0)
         skipCount = skipCount - 1
       else if (skipCount < 1)
         register = instruct.execute(register)
  
}

object testAccumulator extends App {
  // computing 3 * 4 + 9
  Accumulator.program = List(Add(3), Mul(4), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing (((10 * 2) + 3) * 5)
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  println("register = " + Accumulator.register)
  Accumulator.program = List(Add(3), Mul(4), Halt(), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 12
  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 42
  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Blt(33, 2), Add(10), Halt(), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 32

}