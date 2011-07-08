#!/usr/bin/env ruby
#this file implements a grep like tool in ruby

def usage
   puts "USAGE: grep.rb filename substr"
end

if ARGV.length != 2
   usage
else
   File.open(ARGV[0], "r") do |aFile|
      aFile.each_line do |line| 
          if line =~ Regexp.new(ARGV[1]) 
             puts "Got #{line.dump}" 
          end
          # ... process the line
      end
      # ... process the file
   end
end



