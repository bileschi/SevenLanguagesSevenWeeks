List_Builder := Object clone

List_Builder curlyBrackets := method(
  l := List clone
  call message arguments foreach(arg,
     l append(arg)
     )
  l
  )
