l_3 := list(list(1,2,3), list(4,5,6), list(7,8,9))

sum_2d := method( LL, 
  s := 0;
  LL foreach(i, vL, vL foreach(j, v, s = s + v))
)

My2dList := Object clone

My2dList dim := method(m, n, 
	l := List clone
	for (i, 1, m, 
		ll := List clone
		for (i, 1, n, 
			ll append(0)
			)
		l append(ll)
		)
	self data := l
)

My2dList get := method(m, n,
	self data at(m) at(n)
)

ll = My2dList clone