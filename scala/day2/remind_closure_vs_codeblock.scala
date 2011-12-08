// a code block is just a piece of code.  
//  It may be given a name.  It may take a paramter.  It may return a value.
// a code block is not a function.
// a function that has bound to it variable external to that function is known as a closure.
// the meaning of a closure is not well known to java programmers because of scoping issues.  The ability to call
// a function f in scope S in java means that you would also be able to reference variables defined S. 
//
// when functions are passed as objects, this breaks the nesting rules of scoping.  That variables are carried along with
// the function definiton is the closure.

def exploreClosures1():Unit = { // takes nothing returns Unit (Scala for void, but can be instantiated as () )
	println("[exploreClosures1]")
	var y = 0
	def myInternalFunction1(x: Int) : Int = {
		println("[myInternalFunction1]")
		y += 1
		return x * 100 + y
	}
	println("go 1")
	println(myInternalFunction1(3)) // should print 301
	println("go 2")
	println(myInternalFunction1(3)) // should print 302.  Here lies the Java programmers lack of surprise.
}

// invoke the function we have defined above.
exploreClosures1()

def exploreClosures2():Int => Int = { // takes nothing but returns a function from Int to Int
	println("[exploreClosures2]")
	var y = 0
	def myInternalFunction2(x: Int) : Int = {
		println("[myInternalFunction2]")
		y += 1
		return x * 100 + y
	}
	return myInternalFunction2
}

val myFirstClosure = exploreClosures2() // create a closure by grabbing the internal function, and it's y

println("go 1")
println(myFirstClosure(4)) // should print 401
println("go 2")
println(myFirstClosure(4)) // should print 402
// This is what will be confusing to someone unfamilliar with the concept of a closure.  
// myExternalFunction is just a function, but it has the property of an object, in that it carries data along with computation.
// note that we can create two separate functions by calling exploreClosures2 again.  These will have two separate y's
val mySecondClosure = exploreClosures2() // create a closure by grabbing the internal function, and it's y
println("go 1")
println(mySecondClosure(5)) // should print 501

/////////////////////////////////////////////////
// Aside:  how is a function not a code block? //
/////////////////////////////////////////////////
// Cribbed from http://downgra.de/2010/08/05/scala_gotcha_blocks_and_functions/ 

// this is a function, f1, which takes as input another function Int -> Int
def f1(f: Int => Int) = {
       println(">> f1")
       println("  f(23)=" + f(23))
       println("  f(42)=" + f(42))
       println("<< f1")
     }

println(f1({println("I'm part of a code block"); _ * 2})) 
// here I have handed f1 a CODE BLOCK which returns a function Int->Int which doubles integers
// the code block will be executed one time, returning the anonymous function, which is handed to f1.  the output is:
// --- snip ---
//I'm part of a code block
//>> f1
//  f(23)=46
//  f(42)=84
//<< f1
// --- pins ---
def x : Int => Int = { i => println("I'm part of a function"); i * 3 }
println(f1(x)) 
// outputs:
// --- snip --- 
//>> f1
//I'm part of a function
//  f(23)=69
//I'm part of a function
//  f(42)=126
//<< f1
// --- pins --- 
def myCodeBlock{
	println("I am myCodeBlock")
}

