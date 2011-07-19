#!/usr/bin/env ruby
require 'open-uri'
require 'nokogiri'
puts '<html><head></head><body>'
frontpage = Nokogiri::HTML(open("http://www.fark.com"))
File.open('fark_just_imagecomments.html', 'w') do |f2| 
	frontpage.xpath("//tr[@class='headlineRow']").each_with_index do |story, story_i| 
	   unless story_i > 10   
		   sc = story.children
		   storyTitle = sc[4].text.strip
		   unless sc[6].nil?
		      storyCommentLink = sc[6].children[1].attribute('href').text.strip
		      f2.puts '<h1>Title:' + storyTitle + '</h1><br>'
		      f2.puts 'CommentLink:' + storyCommentLink + '<br>'
		      commentsDoc = Nokogiri::HTML(open(storyCommentLink))
		      commentsDoc.xpath("//div/img").each_with_index do |img, i|
		         src = img['src']
		         f2.puts '   Image[' + i.to_s + "] " + src + "<br>"
		         f2.puts '<img src=\''+src+'\'><br>'
		      end
		   end
	   end
	end
	puts '</head></body>'
end
