#!/usr/bin/env python

from random import shuffle
from cards import Card

class Pile(object):
	"""a pile is an ordered collection of cards with an easy shuffling operation
	   it also has utility for counting specific cards etc.  Piles can be uniform
	   or contain multiple different types of cards.

	   Implemented as a list of Cards.
	   """ 
	@classmethod
	def build_from_list(cls, card_list):
		""" argument may be a list of cards, or a list of strings representing
		card names"""
		p = cls()
		for c in card_list:
			if isinstance(c, Card):
				p.add_card_to_pile(c)
			else:
				p.add_new_card_to_pile(c)
		return p

	def __init__(self, name='', count=0):
		self._cards = []
		if count > 0:
			self.add_new_card_to_pile(name, count)

	def __str__(self):
		suffix = "" if len(self._cards) == 1 else "s"
		return "pile with " + str(len(self._cards)) + " card" + suffix

	def __len__(self):
		return len(self._cards)

	def __eq__(self, other):
		""" piles are equal if all their cards are equal and in order.  
		A pile may also equal a list of cards.
		Useful for testing. """
		if len(other) != len(self):
			return False
		if isinstance(other, Pile):
			card_list = other._cards
		else:
			if isinstance(other, list):
				card_list = other
			else:
				return NotImplemented
		for i, card in enumerate(card_list):
			if card != card_list[i]:
				return False
		return True

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

	def count_specific_card(self, card_name):
		c = 0
		target_card = Card(card_name)
		for card in self._cards:
			c += 1 if card == target_card else 0
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
			if card.has_name(card_name):
				return self._cards.pop(i)
		return None

	def shuffle(self):
		"randomly reorders cards in pile"
		shuffle(self._cards)

	def take_top_n(self, n=1):
		"returns a list of n cards off the top of the deck.  The list is not reordered"
		top_n_cards = self._cards[-n:]
		self._cards = self._cards[0:-n]
		return top_n_cards

	def pretty_print(self):
		print str(self)
		for card in self._cards:
			print card

########################################
############# Unit Tests ###############
########################################

def test_init_pile():
	p1 = Pile()
	assert(len(p1) == 0)  # no args -> empty pile
	p2 = Pile(name='copper')
	assert(len(p2) == 0)  # no count arg -> empty pile
	p3 = Pile(name='copper', count = 1234)
	assert(len(p3) == 1234) 
	try:
		p4 = Pile(name='Not a real card name', count = 1)
		assert False
	except KeyError as e:
		pass # building a pile with an unknown card raises an exception

def test_add_card_to_pile():
	p = Pile('copper', 10)
	assert(len(p) == 10)
	p.add_card_to_pile(Card('silver')) # adding a card puts it atop the pile
	assert(len(p) == 11)
	c = p.pop() # pop a card off the top.  It should be the 1 silver
	assert(len(p) == 10)
	assert(c.name == 'silver')	
	p.add_new_card_to_pile(name = 'gold', count = 2) # adding without instantiation	
	assert(len(p) == 12)
	assert(p.pop().name == 'gold')
	assert(p.pop().name == 'gold')
	assert(p.pop().name == 'copper')
	assert(len(p) == 9)

def test_merge_piles():
	p1 = Pile('copper', count=2)
	p2 = Pile('silver', count=2)
	p1.add_others_to_pile(p2) # add another pile
	assert(len(p1) == 4)
	p1.add_others_to_pile([Card('gold'), Card('gold')]) # add cards as list
	assert(p1 == ['gold', 'gold', 'silver', 'silver', 'copper', 'copper'])
	assert(p1.count_specific_card('gold') == 2)
	p1.empty()
	assert(len(p1) == 0)

def test_pull_first_instance_of():
	p = Pile.build_from_list( ['copper', 'silver', 'gold'] )
	c = p.pull_first_instance_of('silver')
	assert(c == Card('silver'))
	assert(p == ['copper', 'gold'])

def test_shuffle():
	"""Builds a large pile, shuffles it, and checks that it has changed.
	Note that this test will pass with high propbability, but fail
	1 out of 2^128 runs."""
	start_order = ['silver', 'gold'] * 64;
	p = Pile.build_from_list( start_order )
	p.shuffle()
	assert(len(p) == len(start_order))
	assert(p != start_order)
	assert(p.count_specific_card('silver') == 64)
	assert(p.count_specific_card('gold') == 64)

def test_take_top_n():
	p = Pile(['copper','silver','gold'])
	top_2 = p.take_top_n(2)
	assert(top_2[0] == Card('silver'))
	assert(top_2[1] == Card('gold'))


########################################
############# Usage      ###############
########################################

if __name__ == '__main__':
	supply = {}
	supply['copper'] = Pile('copper', 30 )
	supply['estate'] = Pile('estate', 30)
	# supply['fake'] = Pile('fake', 1)
	print(str(supply))
	print(str(supply['copper']))
	print(str(supply['estate']))
	# print(str(supply['fake']))
	hand = Pile('copper', 7)
	hand.add_new_card_to_pile('estate', 3)
	print '--------------'
	hand.pretty_print()
	hand.shuffle()
	print '--------------'
	hand.pretty_print()
	print "number of copper: " + str(hand.count_specific_card('copper'))
	print "number of estate: " + str(hand.count_specific_card('estate'))
