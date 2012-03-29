List my_avg := method(
   s = 0
   self foreach(i, v,  s = s + v)
   s / (self size)
)

l := List clone

l append(1)
l append(2)
l append(3)
l append(4)
l my_avg