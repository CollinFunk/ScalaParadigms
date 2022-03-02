package collections.lab4

import scala.annotation.tailrec

object SpellChecker extends App {
  val oed = List("dog", "cat", "bat", "bug", "fox", "see", "run", "bite", "the", "a", "and")
  val essay = "See the blue dog run . See the blue dog bite the man ."

  def toArray(input: String): Array[String] =
    input.replaceAll("[^\\s\\p{L}\\p{Nd}]+", "").toLowerCase.split(("\\s+"))

  var stringArr = toArray(essay)

  def spellCheckPipeline(inString: String, dictonary: List[String]): Int =
    inString.replaceAll("[^\\s\\p{L}\\p{Nd}]+", "").toLowerCase.split(("\\s+")).toList.filter(word => !dictonary.contains(word)).size

  def spellCheckRecursive(inString: String, dictonary: List[String]): Int =
    @tailrec
    def helper(stringArr: Array[String], dictonary: List[String], index: Int, count: Int): Int =
      if (stringArr.size == 0)
        count
      else if (stringArr.size == index)
        count
      else if (!dictonary.contains(stringArr(index)))
        helper(stringArr, dictonary, index + 1, count + 1)
      else
        helper(stringArr, dictonary, index + 1, count)
    helper(inString.replaceAll("[^\\s\\p{L}\\p{Nd}]+", "").toLowerCase.split(("\\s+")), dictonary, 0, 0)


  println(spellCheckRecursive(essay, oed))
  println(spellCheckPipeline(essay, oed))





}
