
Number newdivide := method(other, if (other==0 , 0, self / other ) )

writeln("new divide 4 / 2: ", 4 newdivide(2))
writeln("new divide 4 / 0: ", 4 newdivide(0))

OperatorTable addOperator("newdivide",2)

newdivide := method(other, if (other==0 , 0, self / other ) )

writeln("new divide as operator 3 newdivide 2:", 3 newdivide 2)
writeln("new divide as operator 3 newdivide 0:", 3 newdivide 0)