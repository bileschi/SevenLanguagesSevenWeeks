#!/usr/bin/env python

from cards import *
import pdb

class PlayerCards(object):
	def __init__(self):
		self.reset_empty()
	def reset_empty(self):
		self._deck = Pile()
		self._hand = Pile()
		self._discard = Pile()
		self._in_play = Pile()
	def reset_new_game(self):
		self._deck = Pile('copper',7)
		self._deck.add_new_card_to_pile('estate',3)
		self._deck.shuffle()
		self._hand = Pile()
		self._discard = Pile()
		self._in_play = Pile()
		self.draw_n(5)
	def draw_n(self, n):
		for i in range(0,n):
			self.draw()
	def draw(self):
		""" drawing cards involves recycling the discard deck if necessary.  If there are
		no cards in the deck or discard pile, then draw_to_hand has no effect"""
		if len(self._deck) > 0:
			card = self._deck.pop()
			self._hand.add_card_to_pile(card)
		elif len(self._discard) > 0:
			self.recycle_discard_pile()
			self.draw()
	def recyle_discard_pile(self):
		self._deck.add_other_to_pile(self._discard)
		self._deck.shuffle()
	def pretty_print(self):
		print "----Deck:------",
		self._deck.pretty_print()
		print "----Hand:------",
		self._hand.pretty_print()
		print "----Discard:---",
		self._discard.pretty_print()


################ Definition Complete ###########


# Testing.

if __name__ == "__main__":
	pc = PlayerCards()
	print pc
	print "BEFORE NEW GAME"
	pc.pretty_print()
	print "NEW GAME!"
	pc.reset_new_game()
	pc.pretty_print()

