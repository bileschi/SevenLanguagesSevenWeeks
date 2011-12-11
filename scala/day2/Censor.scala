import scala.collection.mutable.HashMap

def conditionalReplacement(word: String, censorMap: HashMap[String,String]): String = {
  if(censorMap.contains(word)) censorMap(word)
  else word
}

def Censor(in: String, censorMap: HashMap[String, String]):String = {
  val toks = testString.split(" ")
  val filtToks = for (word <- toks) yield conditionalReplacement(word, censorMap)
  filtToks.reduceLeft(_ + " " + _)
}

val censorMap = new HashMap[String, String]
censorMap += "shoot" -> "pucky"
censorMap += "darn" -> "beans"

val testString = "It's time for us to shoot the darn moon"

println(testString)
println(Censor(testString, censorMap))