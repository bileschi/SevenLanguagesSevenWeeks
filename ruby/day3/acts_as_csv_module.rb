
module ActsAsCsv

	def self.included(base)
		base.extend ClassMethods
	end

	module ClassMethods
		def acts_as_csv
			include InstanceMethods
		end
	end

	module InstanceMethods

		class CsvRow
			attr_accessor :myHeaderToInt, :myContent
			def initialize(content, headers)
				@myHeaderToInt = {}
				@myContent = content
				i = 0
				headers.each do |header|
					myHeaderToInt[header] = i
					i += 1
				end
			end
			
			def method_missing name, *args
				@myContent[@myHeaderToInt[name.to_s]]
			end
		end

		def read
			@csv_contents = []
			filename = self.class.to_s.downcase + '.txt'
			file = File.new(filename)
			@headers = file.gets.chomp.split(', ')


			file.each do |row|
				@csv_contents << CsvRow.new(row.chomp.split(', '),headers)
			end
		end

		attr_accessor :headers, :csv_contents

		def initialize
			read
		end

		def each(&block)
			csv_contents.each {|item| block.call(item)}
		end

	end

end

class SmallTest
	include ActsAsCsv
	acts_as_csv
end

m = SmallTest.new
puts m.headers.inspect
puts m.csv_contents.inspect
puts '--- now illustrating each ---'
puts '--- call for header one, should write lions \n bears ---'
m.each {|row| puts row.one}
puts '--- call for header two, should write tigers \n ohmy ---'
m.each {|row| puts row.two}
