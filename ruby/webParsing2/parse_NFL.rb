#!/usr/bin/env ruby
# tool to scrape listing of Yahoo! NFL players to determine player position, team, and visage.   For educational purposes only.

require 'rubygems'
require 'open-uri'
require 'nokogiri'

basedir = '/Users/sbileschi/proj/SevenLanguagesSevenWeeks/ruby/webParsing2/'

htmlFN = 'Tom Brady.html'


basedir = '/Users/sbileschi/proj/SevenLanguagesSevenWeeks/ruby/webParsing2/'
imgWriteDir = '/Users/sbileschi/proj/NFLData/imgs/'
fileWriteDir = '/Users/sbileschi/proj/NFLData/'
htmlFNs = ['NFL_QB.html','NFL_WR.html','NFL_RB.html','NFL_TE.html']
positions = ['QB','WR','RB','TE']
File.open(File.join(fileWriteDir,'my_nfl_db.csv'), 'w') do |fw1|
    positions.each_with_index do |pos,pos_i|
        htmlFN =  htmlFNs[pos_i]
        frontpage = Nokogiri::HTML(open(File.join(basedir,htmlFN)))
        playerRows = frontpage.xpath("//tr[@class='ysprow1'] | //tr[@class='ysprow2']")
        n_playerRows = playerRows.size
        playerRows.each_with_index do |playerRow, playerRow_i| 
            # puts "player" +  playerRow_i.to_s + " of " + n_playerRows.to_s
            playerName = playerRow.children()[(0)].children[1].children[0].text
            teamName = playerRow.children()[(4)].children[0].text
            webSite = playerRow.children()[(0)].children[1]["href"]
            puts "  Getting... Player: " + playerName + "  Team: " + teamName + "  Site: " + webSite
            # fw1.puts playerName + "," + pos + "," + teamName + "," + webSite
            playerdoc = Nokogiri::HTML(open(webSite))
            stringWithImgUrl = playerdoc.xpath("//meta[@property='og:image']")[0]['content']
            # stringWithImgUrl = playerdoc.xpath("//meta")[5]["content"]
            if ((stringWithImgUrl.nil?) | (stringWithImgUrl.split('--/')[2].nil?))
                puts "no image info for " + playerName + "of the " + teamName
                imgName = "NONE"
            else
                imgUrl = stringWithImgUrl.split('--/')[2];
                imgName = imgUrl.split('/')[-1]
                open(imgUrl) do |hnd|
                    File.open(File.join(imgWriteDir,imgName),"wb") {|file| file.puts hnd.read }
                    puts "got "+File.join(imgWriteDir,imgName)
                end
            end
            puts "      [Done] Player: " + playerName + "  Team: " + teamName + "  Site: " + webSite + "  Img: " + imgName
            fw1.puts playerName + "," + pos + "," + teamName + "," + webSite + "," + imgName
        end
    end
end