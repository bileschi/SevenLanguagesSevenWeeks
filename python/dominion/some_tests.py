#!/usr/bin/env python

from game_state import *
import pdb

# Testing.

if __name__ == "__main__":

	### Testing create player cards and shuffle
	pc = PlayerCards()
	print pc
	print "BEFORE NEW GAME"
	pc.pretty_print()
	print "NEW GAME!"
	pc.reset_new_game()
	pc.pretty_print()
	print "copper in hand " + str(pc.count_in_hand('copper'))
	print "estate in hand " + str(pc.count_in_hand('estate'))

	### Testing count 5-2 vs 4-3 initializations
	nCopper = []
	nHands = 12345
	print "determing 5-2 ratio by simulating " + str(12345) + " hands"
	for iTest in range(0,nHands):
		pc.reset_new_game()
		nCopper.append(pc.count_in_hand('copper'))
	countNCopperHands = Counter(nCopper)
	print str(countNCopperHands)
	print "portion of 5-2 starts (should be 0.166...) : " + str((countNCopperHands[5] + countNCopperHands[2]) / float(nHands))

	### Teting Supply
	supply = Supply()
	supply.set_pile('copper', 30)
	supply.set_pile('estate', 30)
	print "i see: " + str(supply.count_pile('copper')) + " copper"
	print "i see: " + str(supply.count_pile('estate')) + " estates"
	for i in range(0, 32):
		print str(supply.take_one('copper'))
	print "i see: " + str(supply.count_pile('copper')) + " copper"

	### Testing two players compete in a very simple game with a very simple controller
	print """A simple game:  only copper and estates.  Estates cost 2.  Player 1 always buys an
	an estate if he can.  Player 2 buys estates if he has 3 or more copper."""
	pc1 = PlayerCards()
	pc2 = PlayerCards()
	pc1.reset_new_game()
	pc2.reset_new_game()
	supply = Supply()
	supply.set_pile('copper', 30)
	supply.set_pile('estate', 30)
	playerOrder = [pc1, pc2]
	nPlayers = len(playerOrder)
	playerNum = 0;
	thresh_to_buy_estate = [2,3]
	while supply.count_pile('estate') > 0:
		print 'round: ' + str(playerNum)
		nextPlayerIndex = playerNum % nPlayers
		nextPlayer = playerOrder[nextPlayerIndex]
		money = nextPlayer.count_in_hand('copper')
		print ' "I am player ' + str(nextPlayerIndex + 1)
		print ' "I have ' + str(money) + ' dollars."',
		if money >= thresh_to_buy_estate[nextPlayerIndex]:
			card = supply.take_one('estate')
		else:
			card = supply.take_one('copper')
		print ' "I buy a ' + str(card) + '."'
		nextPlayer.put_card_on_pile(card, 'discard')
		nextPlayer.put_pile_in_other_pile('hand','discard')
		nextPlayer.put_pile_in_other_pile('in_play','discard')
		nextPlayer.draw_n(5)
		playerNum += 1
	print 'player 1 nVictory = ' + str(pc1.count_in_all('estate'))
	print 'player 2 nVictory = ' + str(pc2.count_in_all('estate'))
	if(pc1.count_in_all('estate') > pc2.count_in_all('estate')):
		print "Player 1 wins!!"
	else:
		print "Player 2 wins!!"









