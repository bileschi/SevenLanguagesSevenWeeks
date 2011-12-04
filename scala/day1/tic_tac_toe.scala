import scala.collection.mutable.HashMap

val boardLocations = Set((1,1),(1,2),(1,3),
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
		println("")
	}
	def locHas(loc: (Int,Int), thisTurnObj: ScalaObject) = {
		return currentBoard(loc) == thisTurnObj;
	}
}

class GameControler {
	val gameModel = new GameModel()
	gameModel.reset()
	def move(loc: (Int,Int), thisTurnObj: ScalaObject):ScalaObject = {
		if( !gameModel.currentBoard.contains(loc)) {
			println("invalid board location")
			return thisTurnObj
		} 
		println(marks)
		println(marks(Mt))
		println(gameModel.currentBoard(loc))
		println(marks(gameModel.currentBoard(loc)))
		println("current val at " + loc + " is " + gameModel.currentBoard(loc))
		if ( Mt == gameModel.currentBoard(loc) ) {
			// nextTurnObj.shout
			println("moved to " + loc)
			gameModel.currentBoard += loc -> thisTurnObj
			return thisTurnObj
		}
	}

}

val myGameModel = new GameModel()
val myGameControler = new GameControler()
myGameControler.move((1,1),Ex)
myGameControler.move((3,4),Ex)
