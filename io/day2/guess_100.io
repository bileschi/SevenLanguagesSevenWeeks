input := File standardInput
goal := Random value(1,100) ceil
guess_num := 0
guesses := 10
win := false

while((guess_num < guesses),   
	guess := input readLine("#{guess_num} Guess the secret number :" interpolate) asNumber
	"you guessed #{guess asString}\n" interpolate print
	if((guess == goal),	
		"you win!\n" print
		win = true
		break
	)
	if((guess > goal), 
		"too big\n" print, 
		"too small\n" print
	)
	guess_num = guess_num + 1
)
if((win not), 
   writeln ("sorry, goal was #{goal}\n" interpolate),
      writeln ("good job guessing #{goal}" interpolate)
)