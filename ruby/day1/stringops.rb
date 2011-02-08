# RegExp
###
h1 = "Hello World!"
h2 = "Hello Ruby!"
t = /Ruby/
h2 =~ t


# Iteration
###
(1..10).each do |i|
  print "this is sentence number " + i.to_s + "\n"
end

# NumberGuesser
##
def numeric?(object)
  true if Float(object) rescue false
end

t = rand(100)
print "guess a positive integer 1-100: "
g = gets()
while g.to_i!=t
  if !(numeric?(g)) then
    print "that\'s a not positive integer!\n"
  elsif g.to_i<t
      print "too low\n"
  else
      print "too high\n"
  end
  g = gets()
  print g + "\n"
end
print "Great!\n"