#!/usr/bin/env python


from cards import *
from pile import *
from collections import Counter

class Supply(object):
	"""Defines the communal cards of the Dominion supply.   
	Implemented as a dictionary of Piles.  Includes
	API for creating, initialzing, and adjusting supply"""

	def __init__(self):
		self.reset_empty()

	def __str__(self):
		return self.pretty_str()

	def has_pile(self, pile_name):
		return pile_name in self._piles

	def count_pile(self, pile_name):
		if self._piles.has_key(pile_name):
			return len(self._piles[pile_name])
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

	def pretty_str(self):
		outstr = ""
		for pile_name, pile in self._piles.iteritems():
			outstr += pile_name + ": " + str(len(pile)) + "\n"
		return outstr

	def put_in_trash(self, card):
		if "trash" not in self._piles:
			self._piles["trash"] = Pile()
		self._piles["trash"].add_card_to_pile(card)

	def count_empty_piles(self):
		"""returns number of empty piles not counting trash"""
		n = 0
		for pile_name, p in self._piles.iteritems():
			if (len(p) == 0) & (pile_name != "trash"): n += 1
		return n

	def pretty_str(self):
		a = []
		for (p_name, p) in self._piles.iteritems():
			a.append("Pile: '%s' contains %d cards." % (p_name, len(p)))
		return "\n".join(a)

########################################
############# Unit Tests ###############
########################################

def build_test_supply_01():
	s = Supply()
	s.set_pile('copper', 30);
	s.set_pile('militia', 10);
	s.set_pile('remodel', 0);
	return s

def test_create_piles():
	s = build_test_supply_01()
	assert(s.count_pile('copper') == 30)
	assert(s.count_pile('militia') == 10)
	assert(s.count_pile('Trash') is None)
	print s


def test_trash_count_is_card_agnostic():
	s = build_test_supply_01()
	s.put_in_trash(Card('copper'))
	s.put_in_trash(Card('gold'))
	assert(s.count_pile('Trash') is 2)
	print s

def test_count_empty_piles():
	s = build_test_supply_01()
	assert(s.count_empty_piles() == 1)
	s.set_pile('copper',0)
	assert(s.count_empty_piles() == 2)

def test_reset_empty():
	s = build_test_supply_01()
	assert(s.count_empty_piles() == 1)
	s.reset_empty()
	assert(s.count_empty_piles() == 0)
	assert(s.count_pile('Trash') is None)

def test_take_one():
	s = build_test_supply_01()
	assert(s.take_one('copper') == Card('copper'))
	assert(s.count_pile('copper') == 29)
	s.put_in_trash(Card('gold'))
	assert(s.take_one('Trash') == Card('gold'))
	assert(s.count_pile('Trash') == 0)

