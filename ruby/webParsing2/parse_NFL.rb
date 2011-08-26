#!/usr/bin/env ruby
require 'rubygems'
require 'open-uri'
require 'nokogiri'

frontpage = Nokogiri::HTML(open("http://www.fark.com"))