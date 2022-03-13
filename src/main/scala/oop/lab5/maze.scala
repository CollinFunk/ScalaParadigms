package oop.lab5
import cui._

class OutOfGas extends UserError("You are out of gas")
class Escaped extends UserError("You have escaped!")

enum Heading:
  case north, south, east, west

class Robot(val name: String) {
  var heading: Heading = Heading.east
  var fuel: Int = 100
  var position: (Int, Int) = (0, 0)

  def move(steps: Int): Unit =
    heading match {
      case Heading.east =>
        this.position = (position(0), position(1) + steps)
      case Heading.west =>
        this.position = (position(0), position(1) - steps)
      case Heading.north =>
        this.position = (position(0) + steps, position(1))
      case Heading.south =>
        this.position = (position(0) - steps, position(1))
    }
    this.fuel = this.fuel - steps
}

object maze extends Console with App {
  val rng = util.Random()
  var exit = (rng.nextInt(10), rng.nextInt(10))
  val robot = Robot("Robbie")

  def distance (p1: (Int, Int), p2: (Int, Int)) =
    val (a, b) = p1
    val (c, d) = p2
    (math.sqrt((a - c) * (a - c) + (b - d) * (b - d))).toInt

  val instruction = "Commands ::= restart | move STEPS | turn HEADING"
  println(instruction)

  override def execute(cmmd: String): String =
    val tokens = cmmd.split("\\s+")
    tokens(0) match {
      case "turn" =>
        tokens(1) match {
          case "north" =>
            if (distance(robot.position, exit) == 0) throw UserError(robot.name + " has already escaped!")
            if (robot.fuel == 0) throw UserError("You are out of gas!")
            robot.heading = Heading.north
            robot.name + " at " + robot.position + " heading " + robot.heading  + " with " +
              robot.fuel + " units of fuel. Distance to goal = " + distance(robot.position, exit)
          case "south" =>
            if (distance(robot.position, exit) == 0) throw UserError(robot.name + " has already escaped!")
            if (robot.fuel == 0) throw UserError("You are out of gas!")
            robot.heading = Heading.south
            robot.name + " at " + robot.position + " heading " + robot.heading  + " with " +
              robot.fuel + " units of fuel. Distance to goal = " + distance(robot.position, exit)
          case "east" =>
            if (distance(robot.position, exit) == 0) throw UserError(robot.name + " has already escaped!")
            if (robot.fuel == 0) throw UserError("You are out of gas!")
            robot.heading = Heading.east
            robot.name + " at " + robot.position + " heading " + robot.heading  + " with " +
              robot.fuel + " units of fuel. Distance to goal = " + distance(robot.position, exit)
          case "west" =>
            if (distance(robot.position, exit) == 0) throw UserError(robot.name + " has already escaped!")
            if (robot.fuel == 0) throw UserError("You are out of gas!")
            robot.heading = Heading.west
            robot.name + " at " + robot.position + " heading " + robot.heading  + " with " +
              robot.fuel + " units of fuel. Distance to goal = " + distance(robot.position, exit)
          case _ => throw UserError("Invalid Heading")
        }
      case "move" =>
        try {
          tokens(1).toInt
        } catch {
          case e: IllegalArgumentException => throw UserError("Steps must be a number")
        }
        if (distance(robot.position, exit) == 0) throw UserError(robot.name + " has already escaped!")
        if (robot.fuel == 0) throw UserError("You are out of gas!")
        robot.move(tokens(1).toInt)
        if (distance(robot.position, exit) == 0)
          robot.name + " has escaped!"
        else
          robot.name + " at " + robot.position + " heading " + robot.heading  + " with " +
            robot.fuel + " units of fuel. Distance to goal = " + distance(robot.position, exit)
      case "restart" =>
        robot.heading = Heading.east
        robot.fuel = 100
        robot.position = (0,0)
        exit = (rng.nextInt(10), rng.nextInt(10))
        robot.name + " at " + robot.position + " heading " + robot.heading  + " with " +
          robot.fuel + " units of fuel. Distance to goal = " + distance(robot.position, exit)
      case _ => throw UserError("Invalid command: " + tokens(0))

    }

  repl // start the repl

}
