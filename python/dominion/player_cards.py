#!/usr/bin/env python

from pile import *
from collections import Counter

class PlayerCards(object):
	""" Defines the structured set of cards which belong to one player.
	Includes, at minimum, the player's 
	(1) hand, 
	(2) deck, 
	(3) discard pile,
	(4) in-play cards
	Provides some basic manipulations for the player, 
	such as drawing cards, shuffling back in his discard pile, 
	and setting a starting deck."""

	def __init__(self):
		self._piles = None
		self.reset_empty()

	def count_in_all(self, card_name):
		""" returns a count of the number of cards with 
		name == card_name in player's deck.  Useful for counting
		the number of victory cards at the end of the game."""
		return sum([self._piles[pile_name].count_specific_card(card_name) 
			for pile_name in self._piles.keys()])

	def count_in_hand(self, card_name=None):
		return self.count_in_pile(card_name, pile_name='hand')

	def count_in_pile(self, card_name=None, pile_name='hand'):
		""" if card_name is defined: returns a count of the 
		number of cards of type card_name in player's associated pile.
		if card_name is None: returns a count of all the cards in the
		pile, without regard to card type """
		if (card_name is None):
			return len(self._piles[pile_name])
		else:
			return self._piles[pile_name].count_specific_card(card_name)

	def draw_n(self, n):
		for i in range(0,n):
			self.draw()

	def draw(self):
		""" drawing cards involves recycling the discard deck if necessary.  If there are
		no cards in the deck or discard pile, then draw has no effect"""
		if len(self._piles['deck']) > 0:
			card = self._piles['deck'].pop()
			self._piles['hand'].add_card_to_pile(card)
		elif len(self._piles['discard']) > 0:
			self.recycle_discard_pile()
			self.draw()

	def get_card_from_pile(self, card, pile_name):
		"""removes and returrns first instance of 'card' from pile_name.  Useful for trashing or discarding.
		Returns the card if found, none if not"""
		return self._piles[pile_name].pull_first_instance_of(card)

	def put_card_on_pile(self, card, pile_name):
		""" adds card to the top of pile with name pile_name """
		self._piles[pile_name].add_card_to_pile(card)

	def put_pile_in_other_pile(self, from_pile, to_pile):
		""" removes cards from from_pile and adds them to to_pile"""
		self._piles[to_pile].add_others_to_pile(self._piles[from_pile])

	def recycle_discard_pile(self):
		""" moves all the cards from discard to deck and shuffles """
		self._piles['deck'].add_others_to_pile(self._piles['discard'])
		self._piles['deck'].shuffle()

	def reset_empty(self):
		self._piles = {}
		self._piles['deck'] = Pile()
		self._piles['hand'] = Pile()
		self._piles['discard'] = Pile()
		self._piles['in_play'] = Pile()

	def reset_new_game(self):
		"""initializes a player hand to look like the beginning of 
		a new games of Dominion"""
		self._piles['deck'] = Pile('copper',7)
		self._piles['deck'].add_new_card_to_pile('estate',3)
		self._piles['deck'].shuffle()
		self._piles['hand'] = Pile()
		self._piles['discard'] = Pile()
		self._piles['in_play'] = Pile()
		self.draw_n(5)

	def pretty_str(self):
		out_str = "----Deck:------\n"
		out_str += self._piles['deck'].pretty_str()
		out_str +=  "\n----Hand:------\n"
		out_str += self._piles['hand'].pretty_str()
		out_str +=  "\n----In-Play:------\n"
		out_str += self._piles['in_play'].pretty_str()
		out_str +=  "\n----Discard:---\n"
		out_str += self._piles['discard'].pretty_str()
		return out_str

	def get_pile(self, pile_name):
		return self._piles[pile_name]

########################################
############# Unit Tests ###############
########################################

expected_piles = ['deck', 'hand', 'discard', 'in_play']

def test_new_playerCards_has_correct_piles():
	pc = PlayerCards();
	assert(pc._piles is not None)
	assert(len(pc._piles) == len(expected_piles))
	assert(pc._piles.has_key(x) for x in expected_piles)

def test_reset_new_game():
	pc = PlayerCards()
	pc.reset_new_game()
	assert(pc.count_in_all('copper') == 7)
	assert(pc.count_in_all('estate') == 3)
	assert(pc.count_in_hand() == 5)
	assert(pc.count_in_hand('copper') <= 5)
	assert(pc.count_in_hand('estate') <= 3)

def test_draw():
	pc = PlayerCards()
	pc.reset_new_game()
	pc.get_pile('deck').add_new_card_to_pile('gold')
	pc.draw()
	assert(pc.count_in_pile('gold') == 1)
	assert(pc.count_in_pile() == 6)
	assert(len(pc._piles['deck']) == 5)

def test_get_card_from_pile():
	pc = PlayerCards()
	pc.reset_new_game()
	c = pc.get_card_from_pile('copper', 'deck')
	print c
	assert(c == Card('copper'))
	c = pc.get_card_from_pile('gold', 'deck')
	assert(c is None)

def test_put_card_on_pile():
	pc = PlayerCards()
	pc.put_card_on_pile(Card('gold'), 'hand')
	assert(pc.count_in_hand() == 1)
	assert(pc.count_in_hand('gold') == 1)
	pc.put_pile_in_other_pile('hand', 'discard')
	assert(pc.count_in_hand('gold') == 0)
	assert(pc.count_in_pile(card_name='gold', pile_name='discard') == 1)

def test_recycle():
	pc = PlayerCards()
	pc.put_card_on_pile(Card('gold'), 'discard')
	pc.put_card_on_pile(Card('silver'), 'discard')
	pc.recycle_discard_pile()
	assert(pc.count_in_pile(card_name='gold', pile_name='deck') == 1)
	assert(pc.count_in_pile(card_name='silver', pile_name='deck') == 1)










