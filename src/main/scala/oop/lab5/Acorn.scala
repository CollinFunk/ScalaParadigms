package oop.lab5

trait Acorn {
  def execute: Double
}

class Num(value: Double) extends Acorn {
  def execute: Double = value
  override def toString: String = value.toString
}

class Sum(val operand1: Acorn, val operand2: Acorn) extends Acorn {
  def execute: Double = operand1.execute + operand2.execute
}

class Prod(val operand1: Acorn, val operand2: Acorn) extends Acorn {
  def execute: Double = operand1.execute * operand2.execute
}


object AcornDemo extends App {
  println(Num(3.14)) // = 3.14
  println(Sum(Num(5), Num(10)).execute) // = 5 + 10 = 15.0
  println(Prod(Num(5), Num(10)).execute) // = 5 * 10 = 50.0
  println(Sum(Num(12), Prod(Num(5), Num(3))).execute) // = 12 + (5 * 3) = 27.0
  println(Prod(Num(7), Sum(Num(8), Num(3))).execute) // = 7 * (8 + 3) = 77.0
}