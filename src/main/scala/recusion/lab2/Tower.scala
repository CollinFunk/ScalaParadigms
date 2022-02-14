package recusion.lab2



import scala.collection.mutable

class Hanoi {
  val numRings = 3
  val numTowers = 3
  val towers: Array[mutable.Stack[Int]] = Array.ofDim[mutable.Stack[Int]](numTowers)
  // initialize towers
  for(t <- 0 until numTowers) towers(t) = mutable.Stack[Int]()
  // push ..., 3, 2, 1 onto tower 0
  for(r <- numRings to 1 by -1) towers(0).push(r) // towers = [Stack(1, 2, 3) Stack() Stack()]

  override def toString = {
    var result = "["
    for(t <- 0 until numTowers) {
      result += towers(t).toString + " "
    }
    result + "]"
  }

  // legally move num rings from fromTower to toTower
  def move(num: Int, fromTower: Int, toTower: Int, byTower : Int): Unit =
    if (num > 0)
      move(num - 1, fromTower, byTower, toTower)
      if (!towers(fromTower).isEmpty && !towers(toTower).isEmpty)
        if (towers(fromTower).top > towers(toTower).top) throw new Exception("Cant put a larger value on a smaller value")
      towers(toTower).push(towers(fromTower).pop())
      println(Tower.game.toString())
      move(num - 1, byTower, toTower, fromTower)
}

object Tower extends App {
  val game = Hanoi()
  println(game.toString)
  // move 3 rings from tower 0 to tower 1
  game.move(3, 0, 2, 1)
}

/*
Output:
[Stack(1, 2, 3) Stack() Stack() ]
[Stack(2, 3) Stack() Stack(1) ]
[Stack(3) Stack(2) Stack(1) ]
[Stack(3) Stack(1, 2) Stack() ]
[Stack() Stack(1, 2) Stack(3) ]
[Stack(1) Stack(2) Stack(3) ]
[Stack(1) Stack() Stack(2, 3) ]
[Stack() Stack() Stack(1, 2, 3) ]
*/
