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
	self m := m
	self n := n
)

My2dList get := method(i, j,
	self data at(i) at(j)
)

My2dList set := method(i, j, v,
	self data at(i) atPut(j,v)
	self data
)

My2dList transpose := method(
	tmp := List clone
	for (i, 0, n-1, 
		ll := List clone
		for (j, 0, m-1, 
			v := self get(j, i)
			ll append(v)
		)
		tmp append(ll)
	)
	self data := tmp
	tmp_m := m
	m = n
	n = tmp_m
)

My2dList pp := method(
	for (i, 0, m-1, 
		for (j, 0, n-1, 
			self get(i, j) print
		)
		"\n" print
	)
)

My2dList write_to_file := method(fn,
# writes 2d list a out as a csv in the format:
# m, n, a(0,0), a(0,1), ... a(m-1,n-1)
	f := File with(fn)
	f remove
	f openForUpdating
	f write(m asString)
	f write(",")
	f write(n asString)
	for (i, 0, m-1, 
		for (j, 0, n-1, 
			f write(",")
			f write(self get(i, j) asString)
		)
	)
	f close
)

My2dList read_from_file := method(fn,
	# read 2d list a from csv in format:
	# m, n, a(0,0), a(0,1), ... a(m-1,n-1)
	f := File with(fn)
	f openForReading
	toks := f readLine split(",")
	f close
	self m := toks at(0) asNumber
	self n := toks at(1) asNumber
	self dim(m,n) # allocate space
	next := 2
	for (i, 0, m-1, 
		for (j, 0, n-1, 
			self set(i,j, toks at(next) asNumber)
			next := next + 1
		)
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

l2 := My2dList clone
ll dim(2,3)
ll set(0,0,1)
ll set(1,1,2)
ll set(1,2,3)
ll write_to_file("test_write.txt")

l2_from_disk := My2dList clone
l2_from_disk read_from_file("test_write.txt")
l2_from_disk pp