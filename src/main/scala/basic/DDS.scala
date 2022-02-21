package basic

import combinators.lab3.DDS

import scala.annotation.tailrec

class DDS {
  //Problem 8.1
  @tailrec
  final def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S =
    if (halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)

  //Problem 8.2
  def populationHalt(pop: Double, x: Int): Boolean =
    pop >= 100000 // 10^5
  def populationUpdate(pop: Int, x: Int): Int =
    pop * 2

  // Problem 8.3
  def solve(f: Double => Double): Double =
    val delta = 1e-7
    def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def df(x: Double) = (f(x + delta) - f(x)) / delta
    def improve(guess: Double, cycle: Int) =
      guess - f(guess)/ df(guess)
    controlLoop(1.0, 0, goodEnough, improve)

  // Problem 8.4
  def sqrt(x: Double) = solve((z: Double) => z * z - x)

  // problem 8.5
  def cubeRoot(x: Double) = solve((z: Double) => z * z * z - x)

  // problem 8.6
  def nthRoot(x: Double, n: Int) = solve((z: Double) => math.pow(z, n) - x)

  //problem 8.7
  //def compoundInterest(period: Int): Double =


}

object TestDDSsd extends DDS with App {
  //problem 2 test
  println(controlLoop[Int](1, 0, populationHalt, populationUpdate))
  //problem 4 test
  println(sqrt(36))
  println(sqrt(49))
  println(sqrt(81))
  //problem 5 test
  println(cubeRoot(216))
  println(cubeRoot(343))
  println(cubeRoot(512))
  //problem 6 test
  println(nthRoot(32, 5))
  println(nthRoot(729, 6))
  println(nthRoot(19683, 9))
}
