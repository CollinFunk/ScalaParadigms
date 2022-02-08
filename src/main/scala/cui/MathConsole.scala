package cui

class MathConsole extends Console {
  def execute(cmmd: String): String = {
    val tokens = cmmd.split("\\s+")
    var result: Double = 0
    if (tokens.length < 2) throw UserError("Provide at least 1 argument")
    tokens(0) match {
      case "add" =>
        for (i <- 1 to tokens.length - 1) {
          result += tokens(i).toDouble
        }
        result.toString
      case "mul" =>
        result = 1
        for (i <- 1 to tokens.length - 1) {
          result *= tokens(i).toDouble
        }
        result.toString
      case "sub" =>
        result = tokens(1).toDouble
        for (i <- 2 to tokens.length - 1) {
          result -= tokens(i).toDouble
        }
        result.toString
      case div =>
        result = tokens(1).toDouble
        for (i <- 2 to tokens.length - 1) {
          if (tokens(i).toDouble == 0) throw UserError("No division by 0")
          result = (result.toFloat / tokens(i).toFloat)
        }
        result.toString
      case _ => throw UserError("Unrecognized operator: " + tokens(0))
    }
  }
}

object MathConsole {
  def main(args: Array[String]): Unit = {
    val cui = MathConsole()
    cui.run(args)
  }
}

/*
Sample output:
->add 2 3 4 5 6
20.0
->mul 4 4 4 4
256.0
->sub
cui.UserError: Provide at least 1 argument
->sub 10 3 2 1
4.0
->div 5 3 .5
3.3333332538604736
->div 20 5 0
cui.UserError: No division by 0
->zip 2 3 4
0.1666666716337204
->quit
bye
*/