#!/usr/bin/env python

from card_prototypes import card_prototypes

class Card(object):
	def __init__(self, name):
		self._name = name
		self._properties = card_prototypes[name];
	def __str__(self):
		return self._name
	def is_treasure(self):
		return self._properties.is_treasure
	def is_action(self):
		return self._properties.is_action
	def is_victory(self):
		return self._properties.is_victory
	def is_curse(self):
		return self._properties.is_curse

################ Unit Tests ###########

def test_making_undefined_card_raises_key_error():
	non_card_name = 'this_is_not_a_real_card_name'
	assert non_card_name not in card_prototypes
	try:
		non_card = Card(non_card_name) # instantiate a non card
		assert False 
	except KeyError as e:
		return
	assert False

def test_making_defined_cards():
	for card_name in card_prototypes.keys():
		card = Card(card_name)


def test_referencing_properties():
	for card_name in card_prototypes.keys():
		card = Card(card_name)
		assert(type(card._properties).__name__ ==  'card_prototype_data') 

################ Usage Examples ###########

if __name__ == "__main__":
	card = Card('copper')
	print card._properties.is_treasure
	for card_name in card_prototypes.keys():
		card = Card(card_name)
		if card.is_treasure():
			print card_name + " is a treasure card costing " + str(card._properties.purchase_cost)
		if card.is_action():
			print card_name + " is an action card costing " + str(card._properties.purchase_cost)
		if card.is_victory():
			print card_name + " is a victory card costing " + str(card._properties.purchase_cost)
		if card.is_curse():
			print card_name + " is a curse card costing " + str(card._properties.purchase_cost)




