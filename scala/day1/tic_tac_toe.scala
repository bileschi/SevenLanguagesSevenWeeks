import scala.collection.mutable.HashMap

val boardLocations = List((1,1),(1,2),(1,3),
						 (2,1),(2,2),(2,3),
						 (3,1),(3,2),(3,3))

val winningSubsets = Set(Set((1,1),(1,2),(1,3)),Set((2,1),(2,2),(2,3)),Set((3,1),(3,2),(3,3)), // rows
						 Set((1,1),(2,1),(3,1)),Set((1,2),(2,2),(3,2)),Set((1,3),(2,3),(3,3)),   // cols
						 Set((1,1),(2,2),(3,3)),Set((1,3),(2,2),(3,1))) // diags
object Ex {}
object Oh {}
object Mt {}
val marks = Map(Ex -> "X", Oh -> "O", Mt -> " ")


class GameModel {
	val currentBoard = new HashMap[(Int,Int),ScalaObject]
	def reset() = {
		boardLocations.foreach( loc => {
			currentBoard += loc ->  Mt
		})
		println("Game Reset")
	}
	def printBoard() = {
		// todo: there must be a more flexible, elegant way to do this
		val s = new StringBuilder()
		s.append(marks(currentBoard((1,1))) + "|")
		s.append(marks(currentBoard((1,2))) + "|")
		s.append(marks(currentBoard((1,3))) + "\n")
		s.append("-----\n")
		s.append(marks(currentBoard((2,1))) + "|")
		s.append(marks(currentBoard((2,2))) + "|")
		s.append(marks(currentBoard((2,3))) + "\n")
		s.append("-----\n")
		s.append(marks(currentBoard((3,1))) + "|")
		s.append(marks(currentBoard((3,2))) + "|")
		s.append(marks(currentBoard((3,3))) + "\n")
		println(s)
	}
	def locHas(loc: (Int,Int), thisTurnObj: ScalaObject):Boolean = {
		return currentBoard(loc) == thisTurnObj;
	}
}

class GameControler {
	val gameModel = new GameModel()
	gameModel.reset()
	def move(loc: (Int,Int), thisTurnObj: ScalaObject):ScalaObject = {
		if( !gameModel.currentBoard.contains(loc)) {
			println("invalid board location")
			printBoard
			return thisTurnObj
		} 
		if ( Mt == gameModel.currentBoard(loc) ) {
			println("moved to " + loc)
			gameModel.currentBoard += loc -> thisTurnObj
			printBoard
			return thisTurnObj
		}
		printBoard
		return thisTurnObj
	}
	def printBoard() = gameModel.printBoard()
}

val myGameModel = new GameModel()
val myGameControler = new GameControler()
myGameControler.move((1,1),Ex)
myGameControler.move((3,4),Oh)
myGameControler.move((1,2),Oh)
myGameControler.move((2,2),Ex)
myGameControler.move((3,3),Oh)


for( ln <- io.Source.stdin.getLines ) {
   var a = (ln.split(",").map( _.toInt))
   var my_move = (a(0),a(1))
   println(my_move)
   myGameControler.move(my_move,Ex)
}
