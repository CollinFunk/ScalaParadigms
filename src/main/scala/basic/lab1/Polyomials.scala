package basic.lab1

class PolynomialUtils {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] =
    var discriminant: Double = (p(1) * p(1)) - 4 * p(0) * p(2)
    if (discriminant > 0)
      var root1: Double = (-p(1) + math.sqrt(discriminant)) / (2 * p(0))
      var root2: Double = (-p(1) - math.sqrt(discriminant)) / (2 * p(0))
      Some(root1, root2)
    else if (discriminant == 0)
      var root1: Double = -p(1) / (2 * p(0))
      Some(root1, root1)
    else
      None

 def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
   var a = 0.0
   var b = 2 * p(0)
   var c = p(1)
   (a, b, c)

  def eval(a: Double, p: (Double, Double, Double)): Double =
    var result : Double = 0
    result = (p(0) * math.pow(a, 2)) + (p(1) * a) + (p(2))
    result

  def toString(p: (Double, Double, Double)) =
    val returnString = new StringBuilder()
    if (p(0) != 0)
      returnString.append(p(0))
      returnString.append("x^2")
    else
      returnString.append("")
    if (p(1) != 0)
      if (p(0) == 0)
        returnString.append("")
      else
        returnString.append(" + ")
      returnString.append(p(1))
      returnString.append("x")
    if (p(2) != 0)
      if (p(1) == 0)
        returnString.append("")
      else
        returnString.append(" + ")
      returnString.append(p(2))
    returnString.toString
}


object Polynomials extends PolynomialUtils with App {
  val poly = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)

  println("poly = " + toString(poly))
  println("eval(6, poly) = " + eval(6, poly))
  println("eval(2, poly) = " + eval(2, poly))
  println("eval(-5, poly) = " + eval(-5, poly))

  println("roots(poly) = " + roots(poly))

  println("deriv(poly) = " + toString(deriv(poly)))
  println("deriv2(poly) = " + toString(deriv(deriv(poly))))

}

/*
output
poly = 3.0x^2 + 9.0x + -30.0
eval(6, poly) = 132.0
eval(2, poly) = 0.0
eval(-5, poly) = 0.0
roots(poly) = Some((2.0,-5.0))
deriv(poly) = 6.0x + 9.0
deriv2(poly) = 6.0
*/