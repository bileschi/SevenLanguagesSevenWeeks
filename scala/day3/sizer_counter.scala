import scala.io._
import scala.actors._
import Actor._
import util.matching.Regex

object PageLoader {
  def getPageSize(url : String) = scala.io.Source.fromURL(url).mkString.length
  def getPageText(url : String) = scala.io.Source.fromURL(url).mkString
  // embarassingly counts using a regular expression on the string version of the html
  // there must be a more robust way.  Creates a fresh regex for every url.
  def getCountPageLinks(url : String) = {
    val text = PageLoader.getPageText(url)
    "<a[ >]".r.findAllIn(text).length
  }
  def getPageLinks(url : String) = {
    // regex cribbed from https://github.com/hbarriuso/seven/blob/master/scala/day3/src/main/scala/Sizer.scala
    val linkRegex = new Regex("(?i)<a href=['|\"]([^'^\"]*)['|\"][^>]*>", "link")
    val text = PageLoader.getPageText(url)
    val matches = linkRegex.findAllIn(text).matchData
    for (amatch <- matches) yield  amatch.group("link")
  }	
}

val urls = List("http://www.amazon.com",
                "http://www.twitter.com",
                "http://www.google.com",
                "http://www.cnn.com")

def timeMethod(method: () => Unit) = {
  val start = System.nanoTime
  method()
  val end = System.nanoTime
  println("Method took " + (end - start) / 1000000000.0 + " seconds")
}

def getPageSizeSequentially() = {
  for(url <- urls) {
    println("SSize for " + url + ": " + PageLoader.getPageSize(url) + ": " + PageLoader.getCountPageLinks(url))
    for(link <- PageLoader.getPageLinks(url)) {
      println(link)
    }
  }
}

def getPageSizeConcurrently() {
  val caller = self
  for (url <- urls) {
    actor {caller ! (url, PageLoader.getPageSize(url), PageLoader.getCountPageLinks(url))}
  }

  for(i <- 1 to urls.size) {
    receive {
    case (url, size1, size2) =>
      println("CSize for " + url + ": " + size1 + ": " + size2)
    }
  }
}

println("Sequential Run:")
timeMethod { getPageSizeSequentially }

println("Current Run:")
timeMethod { getPageSizeConcurrently }