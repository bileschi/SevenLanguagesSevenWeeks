-- my reverse places the last element at the front, and tail recurses the remainder afterwards
myreverse l = last l : (myreverse . init) l
