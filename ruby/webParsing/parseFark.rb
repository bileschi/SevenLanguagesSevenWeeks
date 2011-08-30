#!/usr/bin/env ruby
n_stories = 25
require 'rubygems'
require 'open-uri'
require 'nokogiri'
puts '<html><head></head><body>'
frontpage = Nokogiri::HTML(open("http://www.fark.com"))
File.open('fark_just_imagecomments.html', 'w') do |f2|
	stories = frontpage.xpath("//tr[@class='headlineRow']")
    if n_stories > stories.length 
	    n_stories = stories.length
    end
	stories.each_with_index do |story, story_i| 
	   unless story_i > n_stories
	       puts story_i.to_s + " of " + n_stories.to_s
		   sc = story.children
		   storyTitle = sc[4].text.strip
		   unless sc[6].nil?
		      storyCommentLink = sc[6].children[1].attribute('href').text.strip
		      f2.puts '<h1>Title:' + storyTitle + '</h1><br>'
		      # f2.puts 'CommentLink:' + storyCommentLink 
		      f2.puts '<br>'
		      commentsDoc = Nokogiri::HTML(open(storyCommentLink))
		      commentsDoc.xpath("//div[@class='ctext']/img").each_with_index do |img, i|
		         src = img['src']
		         #f2.puts '   Image[' + i.to_s + "] " + src + "<br>"
		         f2.puts img.parent.text
		         f2.puts '<br>'
		         f2.puts '<img src=\''+src+'\'><br>'
		      end
		   end
	   end
	end
	puts '</head></body>'
end
