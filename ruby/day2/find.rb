# how to access files with and without code blocks

# opening a file without a code block
tempfilename = "./temp.txt"
File.exists?(tempfilename)
myFile = File.open(tempfilename,'w') # returns an IO
myFile.closed?
myFile.close
File.exists?(tempfilename)
File.delete(tempfilename) # returns 1
if File.exists?(tempfilename)
	puts "i already deleted this"
else
	puts "it is gone"
end

#print my own comments
myselfAsCode = File.open('./find.rb');
myselfAsCode.each_line do |line|
	if line =~ Regexp.new("#.*$") 
	 puts "Got #{\1}" 
	end
end


