package cui

import scala.io._

class UserError(gripe: String) extends Exception(gripe)
abstract class Console {

  var verbose = false

  // override in an extension
  def execute(cmmd: String): String


  def run(arguements: Array[String]) : Unit = {
    repl
  }

  def repl: Unit = {
    var more = true
    var cmmd = ""

    while(more && !verbose) {
      try {
        cmmd = StdIn.readLine("->")
        if (cmmd == "quit")
          more = false
        else
          println(execute(cmmd))
      } catch {
        case  e: UserError =>
          println(e)
        case e: Exception => verbose = true
          println(e)
          println(e.printStackTrace)
      }
    }
    println("bye")
  }
}