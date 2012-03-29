#!/usr/bin/env python


from cards import *
from supply import Supply
from collections import Counter
from player_cards import PlayerCards

class GameState(object):
	"""
	Game State holds everything you would need to know to package up an existing Dominion game and start it 
	again later: so long as the game were paused between player turns.
	It does not hold anything about the logic of the game, or how players interact, that belongs elsewhere.
	The game state holds, enumerated
	1. The Supply:  The set of cards from which playsers may gain into their own hands / decks / discard 
	2. The PlayerCards:  Each PlayerCards object holds the state of all of the items which belong to a player, including
	which cards are in his deck, and in which order.  The cards on his discard, in his hand. etc.
	3. History:  The sequence of previous actions 
	4. Player Order: Which players turn is it and who will come next.  and then who.  and then who ...
	"""

	default_kingdom_cards = ["Cellar", "Market", "Militia", "Mine",
	 "Moat", "Remodel", "Smithy", "Village", "Woodcutter", "Workshop"]

	def __init__(self, kingdom_cards = default_kingdom_cards, n_players = 2):
		self._supply = None
		self._players = None
		self._player_order = None
		self._turn_num = 0
		self.reset_new_game(kingdom_cards, n_players);

	def reset_new_game(self, kingdom_cards, n_players):
		# Set up supply
		self._supply = Supply()
		self._supply.set_pile('Copper',30)
		self._supply.set_pile('Silver',30)
		self._supply.set_pile('Gold',30)
		self._supply.set_pile('Curse',30)
		self._supply.set_pile('Estate',8 + 2 * (n_players - 2))
		self._supply.set_pile('Duchy',8 + 2 * (n_players - 2))
		self._supply.set_pile('Province',8 + 2 * (n_players - 2))
		self._supply.set_pile('Trash',0)
		for kingdom_card in kingdom_cards:
			self._supply.set_pile(kingdom_card,10)
		# Set up player cards
		self._players = []
		for i in range(0,n_players):
			new_player = PlayerCards()
			new_player.reset_new_game()
			self._players.append(new_player)
		# Set up player order
		self._player_order = range(0,n_players)
		self._turn_num = 0  # increments every turn.
		# Set up history.  For now implemented as a list of strings.
		self._history = ["Game Initialized"]

	def list_supply_piles(self):
		return [x for x in self._supply._piles.keys()]

	def player_gains_card_from_supply(self, player_index = None, supply_pile_name = None, player_pile_name = "discard"):
		if None == player_index:
			player_index = self.current_player_index()
		self._players[player_index].put_card_on_pile(self._supply.take_one("supply_pile_name"),player_pile_name)

	def player_trashes_card(self, player_index = None, card_name = None, player_pile_name = "hand"):
		if None == player_index:
			player_index = current_player_index()
		card = self._players[player_index].get_card_from_pile(card_name, player_pile_name)
		self._supply.put_in_trash(card)

	def increment_player(self):
		self._turn_num += 1
		self._history.append("Turn " + str(self._turn_num) + ".  Player " + str(self.current_player_index())) 

	def current_player_index(self):
		return self._player_order[self._turn_num % len(self._player_order)]

	def next_player_index(self):
		return self._player_order[(1 + self._turn_num) % len(self._player_order)]

	def prev_player_index(self):
		return self._player_order[(-1 + self._turn_num) % len(self._player_order)]

	def get_player(self, player_index = None):
		if None == player_index:
			player_index = self.current_player_index()
		return self._players[player_index]

	def get_player_pile(self, player_index = None, pile_name = 'hand'):
		if None == player_index:
			player_index = self.current_player_index()
		return self._players[player_index].get_pile(pile_name)

	def get_supply(self):
		return self._supply
################ Definition Complete ###########


# Testing.

if __name__ == "__main__":
	print """play a limited version of dominion with 2 card types using GameState API."""
	my_game_state = GameState()
	print my_game_state.list_supply_piles()
	my_game_state._supply.set_pile('Estate', 30)
	my_game_state._supply.pretty_print()
	thresh_to_buy_estate = [2,3]
	while my_game_state._supply.count_pile('Estate') > 0:
		print 'Turn: ' + str(my_game_state._turn_num)
		player = my_game_state.get_player()
		money = player.count_in_hand('Copper')
		print ' "I am player ' + str(my_game_state.current_player_index() + 1)
		print ' "I have ' + str(money) + ' dollars."',
		if money >= thresh_to_buy_estate[my_game_state.current_player_index()]:
			my_game_state.player_gains_card_from_supply(supply_pile_name = "Estate")
			card = my_game_state._supply.take_one('Estate')
		else:
			my_game_state.player_gains_card_from_supply(supply_pile_name = "Copper")
			card = my_game_state._supply.take_one('Copper')
		print ' "I buy a ' + str(card) + '."'
		player.put_card_on_pile(card, 'discard')
		player.put_pile_in_other_pile('hand','discard')
		player.put_pile_in_other_pile('in_play','discard')
		player.draw_n(5)
		my_game_state.increment_player()
	print 'player 1 nVictory = ' + str(my_game_state._players[0].count_in_all('Estate'))
	print 'player 2 nVictory = ' + str(my_game_state._players[1].count_in_all('Estate'))
	if(my_game_state._players[0].count_in_all('Estate') > my_game_state._players[1].count_in_all('Estate')):
		print "Player 1 wins!!"
	else:
		print "Player 2 wins!!"






