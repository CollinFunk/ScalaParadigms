package basic.lab1


class LinearAlgebraUtils {

  def dim(mat: Array[Array[Double]]): (Int, Int) =
    for(i <- 1 to mat.length - 1)
      if (mat(i).length != mat(i - 1).length) throw new Exception("Rows do not have the same number of values")

    (mat.length, mat(0).length)


  def dot(vec1: Array[Double], vec2: Array[Double]): Double =
    if (vec1.length != vec2.length) throw new Exception("Dimensions don't match")

    var product: Double = 0

    for (i <- 0 to vec1.length - 1) {
      product = product + (vec1(i) * vec2(i))
    }

    product


  def product(mat: Array[Array[Double]], vec: Array[Double]): Array[Double] =
    if (mat(0).length != vec.length) throw new Exception("Dimensions don't match")

    var product = new Array[Double](mat(0).length)
    for (i <- 0 to mat(0).length - 1)
      product(i) = dot(mat(i), vec)
    product




  def transpose(mat: Array[Array[Double]]): Array[Array[Double]] =
    val transposedArray = Array.ofDim[Double](mat(0).length, mat.length)
    for (i <- 0 to mat.length - 1)
      for (j <- 0 to mat(0).length - 1)
        transposedArray(j)(i) = mat(i)(j)
    transposedArray




  def sum(vec1: Array[Double], vec2: Array[Double]): Array[Double] =
    if (vec1.length != vec2.length) throw new Exception("Dimensions don't match")

    var sum = new Array[Double](vec1.length)
    for (i <- 0 to vec1.length - 1)
      sum(i) = vec1(i) + vec2(i)
    sum


  def sum(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]]  =
    if (mat1.length != mat2.length) throw new Exception("Size of matrices dont match")
    if (mat1(0).length != mat2(0).length) throw new Exception("Size of matrices dont match")

    val sumArray = Array.ofDim[Double](mat1.length, mat1(0).length)
    for (i <- 0 to mat1.length - 1)
      for (j <- 0 to mat1(0).length - 1)
        sumArray(i)(j) = mat1(i)(j)  + mat2(i)(j)

    sumArray


  def trace(mat: Array[Array[Double]]) =
    var sum: Double = 0.00
    for (i <- 0 to mat.length - 1)
      sum += mat(i)(i)
    sum

  def toString(vec: Array[Double]): String =
    var newString = "["
    var addedString = vec.mkString(" ")
    var returnString = newString.concat(addedString.concat("]"))
    returnString

  def toString(mat: Array[Array[Double]]): String =
    for(i <- 1 to mat.length - 1)
      if (mat(i).length != mat(i - 1).length) throw new Exception("Rows do not have the same number of values")

    val returnString = new StringBuilder("[")
    for (i <- 0 to mat.length - 1)
      for (j <- 0 to mat(0).length - 1)
        returnString.append(mat(i)(j))
        if (j != mat(0).length - 1)
          returnString.append(" ")
        else
          returnString.append("]")
          returnString.append(System.getProperty("line.separator"))
          if (i != mat.length - 1)
            returnString.append("[")
    returnString.toString
}

object LinearAlgebra extends LinearAlgebraUtils with App {
  try {
    def vec1 = Array(5.0, 2.0, 6.0)

    def mat1 = Array(Array(1.0, 2.0, 3), Array(4.0, 5, 6.00), Array(7.0, 8, 9))

    def mat2 = Array(Array(10.0, 11, 12), Array(13.0, 14, 15), Array(16.0, 17, 18))

    println("vec1 = " + toString(vec1))
    println("mat1 = ")
    println(toString(mat1))
    println("mat2 = ")
    println(toString(mat2))
    println("dim(mat1) = " + dim(mat1))
    println("trace(mat1) = " + trace(mat1))
    println("transpose(mat2) = ")
    println(toString(transpose(mat2)))
    println("sum(mat1, mat2) = ")
    println(toString(sum(mat1, mat2)))
    println("product(mat1, vec1) = " + toString(product(mat1, vec1)))

    val vec2 = Array(0.0, 4.0, 9.0, 8.0)
    println("product(mat1, vec1) = " + toString(product(mat1, vec2)))
  } catch {
    case e: Exception => println(e)
  }

}

/*
output:
vec1 = [5.0 2.0 6.0]
mat1 =
[1.0 2.0 3.0]
[4.0 5.0 6.0]
[7.0 8.0 9.0]

mat2 =
[10.0 11.0 12.0]
[13.0 14.0 15.0]
[16.0 17.0 18.0]

dim(mat1) = (3,3)
trace(mat1) = 15.0
transpose(mat2) =
[10.0 13.0 16.0]
[11.0 14.0 17.0]
[12.0 15.0 18.0]

sum(mat1, mat2) =
[11.0 13.0 15.0]
[17.0 19.0 21.0]
[23.0 25.0 27.0]

product(mat1, vec1) = [27.0 66.0 105.0]
java.lang.Exception: Dimensions don't match
*/
