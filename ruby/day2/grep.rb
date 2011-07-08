#!/usr/bin/env ruby
#this file implements a grep like tool in ruby

def usage
   puts "USAGE: grep.rb filename"
end

if ARGV.length != 1
   usage
else
  ARGV.each do|a|
    puts "Argument: #{a}"
  end
end



