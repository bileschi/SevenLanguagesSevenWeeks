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
	def locHas(loc: (Int,Int), player: ScalaObject):Boolean = {
		return currentBoard(loc) == player;
	}
	def isWinForPlayer(player: ScalaObject):Boolean = {
		winningSubsets.exists(subset => subset.forall(pos => currentBoard(pos) == player))
	}
	def isDraw():Boolean = {
		!boardLocations.exists(loc => currentBoard(loc) == Mt)
	}
}

class GameControler {
	val gameModel = new GameModel()
	gameModel.reset()
	def move(loc: (Int,Int), player: ScalaObject):Boolean = {
		if( !gameModel.currentBoard.contains(loc)) {
			println("invalid board location")
			return false
		} 
		if ( Mt == gameModel.currentBoard(loc) ) {
			println("moved to " + loc)
			gameModel.currentBoard += loc -> player
			return true
		} else {
			return false
		}
	}
	def printBoard() = gameModel.printBoard()
	def isWinForPlayer(player: ScalaObject):Boolean = gameModel.isWinForPlayer(player)
	def reset() = gameModel.reset()
	def isDraw():Boolean = gameModel.isDraw()
}

val myGameControler = new GameControler()
myGameControler.printBoard
var players = List(Ex, Oh)


println("Welcome to Tic-Tac-Toe!")
println("please type your plays as integer coordinates like '1,1'  or '3,2'")
// main game loop.
while(true) {
	print(marks(players(0)) + "'s turn: ")
	val ln = Console.readLine
	if(ln.length() > 0) {
	   var a = (ln.split(",").map( _.toInt))
	   var my_move = (a(0),a(1))
	   println(my_move)
	   // if move is a success, then it's the other players turn
	   if(myGameControler.move(my_move,players(0))) {
	   		players = players.reverse
	   } else {
		   println("error making move, try again.  something like '1,2' (no quotes) ")
	   }
	   myGameControler.printBoard
	   if(myGameControler.isWinForPlayer(Ex)){ 
		   println("XXXXXXXXXXXXXX")
		   println("X is a winner!")
		   println("XXXXXXXXXXXXXX")
		   myGameControler.reset
		   myGameControler.printBoard
		}
	   if(myGameControler.isWinForPlayer(Oh)){ 
		   println("OOOOOOOOOOOOOO")
		   println("O is a winner!")
		   println("OOOOOOOOOOOOOO")
		   myGameControler.reset
		   myGameControler.printBoard
		}
		if(myGameControler.isDraw()) {
			println("\nDraw\n")
		  	myGameControler.reset
			myGameControler.printBoard
		}
	}
}







