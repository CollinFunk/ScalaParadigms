package combinators.lab3

class DDS {
  // problem 1
  final def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
    if (halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)

  // problem 2
  val population = controlLoop[Int](1, 0, populationHalt, populationUpdate)
  def populationHalt(pop: Double, x: Int): Boolean =
    pop >= 100000 // 10^5
  def populationUpdate(pop: Int, x: Int): Int =
    pop * 2

  // problem 3
  def solve(f: Double=> Double): Double =
    val delta = 1e-7
    def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def df(x: Double) = (f(x + delta) - f(x)) / delta
    def improve(guess: Double, cycle: Int) =
      guess - f(guess)/ df(guess)
    controlLoop(1.0, 0, goodEnough, improve)

  // problem 4
  def sqrt(x: Double) = solve((z: Double) => z * z - x)

  // problem 5
  def cubeRoot(x: Double) = solve((z: Double) => z * z * z - x)

  // problem 6
  def nthRoot(x: Double, n: Int) = solve((z: Double) => math.pow(z, n) - x)

  // problem 7

  // = value of an investment of $principle at an anual rate r compounded
  // periods times over 1 year
  def value(principle: Double, rate: Double, periods: Int): Double =
    def end(currentVal: Double, cycle: Int) : Boolean =
      cycle == periods
    def compound(currentVal: Double, cycle: Int): Double =
      currentVal + (rate * currentVal)
    controlLoop(1.0, 0, end, compound)

}

object TestDDS extends DDS with App {
  println(sqrt(49))
  println(sqrt(36))
  println(sqrt(81))
  println(population)
  println(cubeRoot(216))
  println(cubeRoot(343))
  println(cubeRoot(512))
  println(nthRoot(32, 5))
  println(nthRoot(729, 6))
  println(nthRoot(19683, 9))
  println(value(1, .05, 12))
}


