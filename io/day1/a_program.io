"hello_world\n" print
if(nil, "nil is true\n", "nil is false\n") print

Greeter := Object clone
Greeter greet := method("Welcome to IO, dude!\n" print)
Greeter greet