package collections.lab4

object SpellChecker extends App {
  val oed = List("dog", "cat", "bat", "bug", "fox", "see", "run", "bite", "the", "a", "and")
  val essay = "See the blue dog run . See the blue dog bite the man ."
  
  def toArray(input: String): Array[String] =
    input.replaceAll("[^\\s\\p{L}\\p{Nd}]+", "").toLowerCase.split(("\\s+"))

  var stringArr = toArray(essay)

  for (i <- 0 to stringArr.length - 1)
    println(stringArr(i))

  //def spellCheckPipeline(inString: String, dictonary: List[String]): Int =

    





}
