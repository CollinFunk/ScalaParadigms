package recusion.lab2

object Tournament extends App {

  // prob A wins if A needs n wins and B needs m
  def probability(n: Int, m: Int): Double =
    var winningTeam = probabilityHelper((n + m) - 1, n)
    var losingTeam = probabilityHelper((n + m) - 1, m)

    winningTeam * math.pow(.5, 7) + losingTeam * math.pow(.5, 7)




  def probabilityHelper(n: Int, m: Int): Double =
    if (m == 0)
      1
    else
      (n * probabilityHelper(n - 1, m - 1)) / m


  println(probabilityHelper(4, 1))
  // Team A sweeps the World Series
  for(i <- 4 to 0 by -1)
    println("probability A wins = " + probability(i, 4))
}

/*
Output

probability A wins = 0.5
probability A wins = 0.65625
probability A wins = 0.8125
probability A wins = 0.9375
probability A wins = 1.0

*/
