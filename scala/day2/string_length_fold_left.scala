val myListOfStrings = List("my first string", "my second string", "my third string")

def lengthUsingFoldLeft(l: List[String]):Int = {
	l.foldLeft(0)((sum, value) => sum + value.size)
}

println(lengthUsingFoldLeft(myListOfStrings))






