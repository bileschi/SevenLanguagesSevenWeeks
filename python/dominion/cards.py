#!/usr/bin/env python

# classes for card and pile
# a pile is an ordered collection of cards with an easy shuffling operation

from random import shuffle
from card_prototypes import card_prototypes

class Card(object):
	def __init__(self, name):
		self._name = name
		self._properties = card_prototypes[name];
	def __str__(self):
		return self._name

class Pile(object):
	def __init__(self, name='', count=0):
		self._cards = []
		if count > 0:
			self.add_new_card_to_pile(name, count)
	def add_card_to_pile(self, card):
		if card is not None:
			self._cards.append(card)
	def add_new_card_to_pile(self, name, count=1):
		self._cards.extend([Card(name) for i in range(0,count)])
	def add_others_to_pile(self, other_pile):
		""" takes all the cards out of other_pile and adds them in order to this pile.
		should work with other_pile = Pile instance or other_pile = List(Card) """
		try:
			self._cards.extend(other_pile._cards)
			other_pile.empty()
		except AttributeError:
			self._cards.extend(other_pile)
			other_pile = []
	def count_any_card(self):
		return len(self._cards)
	def count_specific_card(self, card_name):
		c = 0
		for card in self._cards:
			c += 1 if card._name == card_name else 0
		return c
	def empty(self):
		""" removes all cards from this pile """
		self._cards = []
	def pop(self):
		"returns a single card off the top of the deck"
		return self._cards.pop()
	def pull_first_instance_of(self, card_name):
		"""removes first instance of 'card' from pile_name.  If there aren't any, returns None"""
		for i, card in enumerate(self._cards):
			if card_name == card._name:
				return self._cards.pop(i)
		return None
	def shuffle(self):
		"randomly reorders cards in pile"
		shuffle(self._cards)
	def take_top_n(self, n=1):
		"returns a list of n cards off the top of the deck.  The list is not reordered"
		top_n_cards = self._cards[-n:]
		self._cards = self.cards[0:-n]
		return top_n_cards
	def pretty_print(self):
		print str(self)
		for card in self._cards:
			print card
	def __str__(self):
		suffix = "" if len(self._cards) == 1 else "s"
		return "pile with " + str(len(self._cards)) + " card" + suffix
	def __len__(self):
		return len(self._cards)


################ Definition Complete ###########


# Testing.
if __name__ == "__main__":
	supply = {}
	supply['copper'] = Pile('copper', 30 )
	supply['estate'] = Pile('estate', 30)
	supply['fake'] = Pile('fake', 1)
	print(str(supply))
	print(str(supply['copper']))
	print(str(supply['estate']))
	print(str(supply['fake']))
	hand = Pile('copper', 7)
	hand.add_new_card_to_pile('estate', 3)
	print '--------------'
	hand.pretty_print()
	hand.shuffle()
	print '--------------'
	hand.pretty_print()
	print "number of copper: " + str(hand.count_specific_card('copper'))
	print "number of estate: " + str(hand.count_specific_card('estate'))




