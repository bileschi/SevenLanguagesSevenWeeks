l_3 := list(list(1,2,3), list(4,5,6), list(7,8,9))

sum_2d := method( LL, 
  s := 0;
  LL foreach(i, vL, vL foreach(j, v, s = s + v))
)

My2dList := Object clone

# method dim creates an internal slot 'data'
# data contains a list of lists of zeros
My2dList dim := method(m, n, 
	l := List clone
	for (i, 1, m, 
		ll := List clone
		for (j, 1, n, 
			ll append(0)
		)
		l append(ll)
	)
	self data := l
)

My2dList get := method(m, n,
	self data at(m) at(n)
)

My2dList set := method(m, n, v,
	self data at(m) atPut(n,v)
	self data
)

My2dList transpose := method(
	tmp := List clone
	m := self data size
	n := self data at(0) size
	for (i, 0, n-1, 
		ll := List clone
		for (j, 0, m-1, 
			v := self get(j, i)
			ll append(v)
		)
		tmp append(ll)
	)
	self data := tmp
)

My2dList pp := method(
	m := self data size
	n := self data at(0) size
	for (i, 0, m-1, 
		for (j, 0, n-1, 
			self get(i, j) print
		)
		"\n" print
	)
)



ll := My2dList clone

"-- initializing array --" println
ll dim(3,4)
ll pp
"-- initializing setting (1,2) <- 5 --" println
ll set(1,2,5)
ll pp
"-- get(1,2) = " print
ll get(1,2) println
"-- transposing --" println
ll transpose
ll pp
"-- get(1,2) = " print
ll get(1,2) println
"-- get(2,1) = " print
ll get(2,1) println
