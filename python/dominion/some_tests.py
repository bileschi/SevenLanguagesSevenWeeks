#!/usr/bin/env python

from cards import *
from collections import Counter
import pdb

class Supply(object):
	"""Defines the communal cards of the Dominion supply.   Implemented as a dictionary of Piles.  Includes
	API for creating, initialzing, and adjusting supply"""
	def __init__(self):
		self.reset_empty()
	def count_pile(self, pile_name):
		if self._piles.has_key(pile_name):
			return self._piles[pile_name].count_any_card()
		return None
	def reset_empty(self):
		self._piles = {}
	def set_pile(self, pile_name, pile_count):
		""" add or adjust a pile count """
		self._piles[pile_name] = Pile(pile_name, pile_count)
	def take_one(self, pile_name):
		"""returns the top card if there is any, otherwise returns None"""
		if(self.count_pile(pile_name) > 0):
			return self._piles[pile_name].pop()
		else:
			return None


class PlayerCards(object):
	""" Defines the structured set of cards which belong to one player.  Includes, at minimum the player's hand,
	his deck, and his discard pile.  Provides some basic manipulations for the player, such as drawing cards, 
	shuffling back in his discard pile, and setting a starting deck."""
	def __init__(self):
		self.reset_empty()
	def count_in_all(self, card_name):
		""" returns a count of the number of cards with name == card_name in player's deck.  Useful for counting
		the number of victory cards at the end of the game."""
		return sum([self._piles[pilename].count_specific_card(card_name) for pilename in self._piles.keys()])
	def count_in_hand(self, card_name):
		" returns a count of the number of cards with name == card_name in player's hand"
		return self._piles['hand'].count_specific_card(card_name)
	def draw_n(self, n):
		for i in range(0,n):
			self.draw()
	def draw(self):
		""" drawing cards involves recycling the discard deck if necessary.  If there are
		no cards in the deck or discard pile, then draw_to_hand has no effect"""
		if len(self._piles['deck']) > 0:
			card = self._piles['deck'].pop()
			self._piles['hand'].add_card_to_pile(card)
		elif len(self._piles['discard']) > 0:
			self.recycle_discard_pile()
			self.draw()
	def put_card_on_pile(self, card, pile_name):
		""" adds card to the top of pile with name pile_name """
		self._piles[pile_name].add_card_to_pile(card)
	def put_pile_in_other_pile(self, from_pile, to_pile):
		""" removes cards from from_pile and adds them to to_pile"""
		self._piles[to_pile].add_others_to_pile(self._piles[from_pile])
	def recycle_discard_pile(self):
		self._piles['deck'].add_others_to_pile(self._piles['discard'])
		self._piles['deck'].shuffle()
	def reset_empty(self):
		self._piles = {}
		self._piles['deck'] = Pile()
		self._piles['hand'] = Pile()
		self._piles['discard'] = Pile()
		self._piles['in_play'] = Pile()
	def reset_new_game(self):
		self._piles['deck'] = Pile('copper',7)
		self._piles['deck'].add_new_card_to_pile('estate',3)
		self._piles['deck'].shuffle()
		self._piles['hand'] = Pile()
		self._piles['discard'] = Pile()
		self._piles['in_play'] = Pile()
		self.draw_n(5)
	def pretty_print(self):
		print "----Deck:------",
		self._piles['deck'].pretty_print()
		print "----Hand:------",
		self._piles['hand'].pretty_print()
		print "----Discard:---",
		self._piles['discard'].pretty_print()
################ Definition Complete ###########


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









