#!/usr/bin/env ruby
require 'rubygems'
require 'open-uri'
require 'nokogiri'


basedir = '/Users/sbileschi/proj/SevenLanguagesSevenWeeks/ruby/webParsing2/'
imgWriteDir = '/Users/sbileschi/proj/NFLData/imgs/'

htmlFN = 'Tom Brady.html'
playerdoc = Nokogiri::HTML(open(File.join(basedir,htmlFN)))
stringWithImgUrl = playerdoc.xpath("//meta")[5]["content"]
imgUrl = stringWithImgUrl.split('--/')[2];
imgName = imgUrl.split('/')[-1]
open(imgUrl) do |hnd|
  File.open(File.join(imgWriteDir,imgName),"wb") {|file| file.puts hnd.read }
  puts "got "+File.join(imgWriteDir,imgName)
end
