=begin
* print the contents of an array of sixteen numbers, 
4 numbers ata  time, using just each.  
now do the  same with each_slice in Enumerable
=end

#Create Array
a=[1,1]
(3..16).each{|i|a.push(a[i-3]+a[i-2]	)} # create an array holding the fibs 1..16

puts "Using just each"
tmp = []
a.each do |i|
  tmp.push(i)
  if tmp.length == 4
    p tmp
    tmp = []    
  end
end

puts "Using each_slice"
require "enumerator"         # necessary for my irb 1.8.6 OSX
a.each_slice(4){|i| p i}

=begin
OUTPUT



/OUTPUT
=end