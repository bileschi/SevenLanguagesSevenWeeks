import scala.io._
import scala.actors._
import Actor._

object PageLoader {
  def getPageSize(url : String) = scala.io.Source.fromURL(url).mkString.length
  def getPageText(url : String) = scala.io.Source.fromURL(url).mkString
  // embarassingly counts using a regular expression on the string version of the html
  // there must be a more robust way
  def getCountPageLinks(url : String) = {
    val text = PageLoader.getPageText(url)
    "<a[ >]".r.findAllIn(text).length
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