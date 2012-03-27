FibGen := Object clone
fib_rec := method (n, 
  if((n <= 2), 1, fib_rec(n-1) + fib_rec(n-2)))

for(i, 1, 20, 1, writeln(i, ":", fib_rec(i) ) )

fib_iter := method(n,
  scratch := list(1, 1)
  for(i, 3, n, 1, 
    scratch append((scratch at(i-2)) + (scratch at (i-3))))
  scratch at(n-1))

for(i, 1, 20, 1, writeln(i, ">", fib_iter(i) ) )
