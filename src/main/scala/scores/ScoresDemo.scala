package scores

// this is how one declares a class with three fields in Scala:
class Note(val amplitude: Double, val frequency: Double, val duration: Double = 1.0)

// a sample score for testing purposes:
val symphony1 =
  List(Note(3, 30), Note(3.1, 40, .25), Note(3.2, 10, .5),
    Note(5.1, 5, -.75), Note(3.9, 2))

def duration1(score: List[Note]) = {
  var result = 0.0
  for(note <- score if 0 < note.duration)
    result = result + note.duration
  result
}


def duration2(score: List[Note]):Double =
  if (score == Nil) 0.0
  else if (0 < score.head.duration) score.head.duration + duration2(score.tail)
  else duration2(score.tail)


def duration3(score: List[Note]) = {
  def helper(result: Double, unseen: List[Note]): Double =
    if (unseen == Nil) result
    else if  (0 < unseen.head.duration)
      helper(result + unseen.head.duration, unseen.tail)
    else helper(result, unseen.tail)
  helper(0.0, score)
}


def sum(a: Double, b: Double) = a + b

def getDuration(n: Note) = n.duration

def isPositive(dur: Double) = 0 < dur


def  duration4(score: List[Note]) =
  score.map(getDuration).filter(isPositive).reduce(sum)


def  duration(score: List[Note]) =
  score.map(_.duration).filter(0 < _).reduce(_ + _)

object ScoresDemo extends App {
  println(duration1(symphony1))
  println(duration2(symphony1))
  println(duration3(symphony1))
  println(duration4(symphony1))
}
