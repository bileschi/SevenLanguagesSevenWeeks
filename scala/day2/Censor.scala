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

// now do it again loading the censor map from disk
val source = scala.io.Source.fromFile("/Users/stanleybileschi/proj/SevenLanguagesSevenWeeks/scala/day2/censorList.txt")
//val source = scala.io.Source.fromFile("censorList.txt")
val lines = source.mkString
source.close()
val x = lines.split(", ")
val censorMap2 = new HashMap[String, String]
for (i <- List.range(0, x.size) if i % 2 == 0) {
   censorMap2 += x(i) -> x(i+1)
}
println(Censor(testString, censorMap2))



