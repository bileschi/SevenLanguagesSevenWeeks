#!/usr/bin/env python


from cards import *
from supply import Supply
from collections import Counter
from player_cards import PlayerCards

class GameState(object):
	"""
	Game State holds everything you would need to know 
	to package up an existing Dominion game and start it 
	again later: so long as the game were paused _between_ 
	player turns.
	
	It does not hold anything about the logic of the game, 
	or how players interact, that belongs elsewhere.
	
	The game state holds:

	1. The Supply:  The set of cards from which playsers 
	may gain into their own hands / decks / discard 
	2. The PlayerCards:  Each PlayerCards object holds 
	the state of all of the items which belong to a player, 
	including which cards are in his deck, and in which order, 
	the cards on his discard, in his hand. etc.
	3. History:  The sequence of previous actions 
	4. Player Order: Which players turn is it and who will 
	come next.  and then who.  and then who ...
	"""

	default_kingdom_cards = ["cellar", "market", "militia", "mine",
	 "moat", "remodel", "smithy", "village", "woodcutter", "workshop"]
	default_treasure_cards = ['copper',  'silver', 'gold']
	default_victory_cards = ['estate',  'duchy', 'province']

	def __init__(self,
	 kingdom_cards = default_kingdom_cards,
	 treasure_cards = default_treasure_cards,
	 victory_cards = default_victory_cards,
	 n_players = 2):
		self._supply = None
		self._player_cards = None
		self._player_order = None
		self._turn_num = 0
		self.reset_new_game(n_players, kingdom_cards);

	def __str__(self):
		return self.pretty_str()

	def reset_new_game(self,
	 n_players,
	 kingdom_cards = default_kingdom_cards,
	 treasure_cards = default_treasure_cards,
	 victory_cards = default_victory_cards):
		# Set up supply
		self._supply = Supply()
		for card_name in treasure_cards:
			self._supply.set_pile(card_name, 30)
		for card_name in victory_cards:
			self._supply.set_pile(card_name, 4 + 2 * n_players)
		for card_name in kingdom_cards:
			self._supply.set_pile(card_name, 10)
		self._supply.set_pile('curse',30)
		self._supply.set_pile('trash',0)
		# Set up player cards
		self._player_cards = []
		for i in range(0,n_players):
			new_player = PlayerCards()
			new_player.reset_new_game()
			self._player_cards.append(new_player)
		# Set up player order
		self._player_order = range(0,n_players)
		self._turn_num = 0  # increments every turn.
		# Set up history.  For now implemented as a list of strings.
		self._history = ["Game Initialized"]

	def list_supply_piles(self):
		return [x for x in self._supply._piles.keys()]

	def supply_has_pile(self, pile_name):
		return self._supply.has_pile(pile_name)

	def supply_count_pile(self, pile_name):
		return self._supply.count_pile(pile_name)

	def player_gains_card_from_supply(
		self, 
		player_index = None, 
		supply_pile_name = None, 
		player_pile_name = "discard"):
		""" card is removed from named supply pile, and placed into the
		player's pile.  Which player defaults to current player.
		"""
		if None == player_index:
			player_index = self.current_player_index()
		card = self._supply.take_one(supply_pile_name);
		self._player_cards[player_index].put_card_on_pile(
			card, player_pile_name)

	def player_trashes_card(
		self, 
		player_index = None, 
		card_name = None, 
		player_pile_name = "hand"):
		""" card is removed from players cards, and put atop trash pile.
		which player defaults to current player """
		if None == player_index:
			player_index = self.current_player_index()
		card = self._player_cards[player_index].get_card_from_pile(
			card_name, player_pile_name)
		self._supply.put_in_trash(card)

	def increment_player(self):
		""" increments internal _turn_num variable. """
		self._turn_num += 1
		self._history.append(
			"Turn " + str(self._turn_num) + 
			".  Player " + str(self.current_player_index())) 

	def current_player_index(self):
		return self._player_order[self._turn_num % len(self._player_order)]

	def next_player_index(self):
		return self._player_order[(1 + self._turn_num) % len(self._player_order)]

	def prev_player_index(self):
		return self._player_order[(-1 + self._turn_num) % len(self._player_order)]

	def get_player(self, player_index = None):
		if None == player_index:
			player_index = self.current_player_index()
		return self._player_cards[player_index]

	def get_player_pile(self, player_index = None, pile_name = 'hand'):
		if None == player_index:
			player_index = self.current_player_index()
		return self._player_cards[player_index].get_pile(pile_name)

	def get_supply(self):
		return self._supply

	def pretty_str(self):
		strs = []
		strs.append("Game State Supply:")
		strs.append(str(self._supply.pretty_str()))
		strs.append("Game State Player_card List:")
		for i, pc in enumerate(self._player_cards):
			strs.append( "player %d" % i)
			strs.append(str(pc.pretty_str()))
		strs.append("Game State Player Order:")
		strs.append(str(self._player_order))
		strs.append("Game State Turn Num:")
		strs.append(str(self._turn_num))
		return "\n".join(strs)

################ Definition Complete ###########

########################################
############# Unit Tests ###############
########################################

def test_init_game_state():
	gs = GameState()
	for pile_name in GameState.default_kingdom_cards:
		assert(gs._supply.has_pile(pile_name))
	assert(gs.supply_has_pile('copper'))
	assert(gs.supply_has_pile('silver'))
	assert(gs.supply_has_pile('gold'))
	assert(gs.supply_has_pile('estate'))
	assert(gs.supply_has_pile('duchy'))
	assert(gs.supply_has_pile('province'))
	assert(gs.supply_has_pile('curse'))
	assert(gs.supply_has_pile('trash'))
	assert(not gs.supply_has_pile('mysterious pants'))
	assert(len(gs.get_player_pile(0,'hand')) == 5)
	assert(len(gs.get_player_pile(1,'hand')) == 5)
	assert(len(gs._player_order) == 2)
	print gs._player_order
	assert(gs._player_order.index(0) == 0)
	assert(gs._player_order.index(1) == 1)
	assert(gs.current_player_index() == 0)
	assert(gs.next_player_index() == 1)
	assert(gs.prev_player_index() == 1)

def test_list_piles():
	gs = GameState()
	l = gs.list_supply_piles()
	d = set()
	for p in l:
		d.add(p)
	for p in GameState.default_kingdom_cards:
		assert p in d

def test_player_gains_card_from_suply():
	gs = GameState()
	assert(gs.current_player_index() == 0)
	gs.player_gains_card_from_supply(supply_pile_name = 'gold', player_pile_name='hand')
	assert(gs.get_player().count_in_pile(pile_name='hand') == 6)
	assert(gs.get_player().count_in_pile(card_name='gold', pile_name='hand') == 1)

def test_player_trashes_card():
	gs = GameState()
	assert(gs.current_player_index() == 0)
	gs.player_gains_card_from_supply(supply_pile_name = 'curse', player_pile_name='hand')
	gs.player_trashes_card(card_name = 'curse', player_pile_name = 'hand')
	print str(gs)
	assert(gs.get_player().count_in_pile(pile_name='hand') == 5)
	assert(gs.get_player().count_in_pile(card_name='curse', pile_name='hand') == 0)
	assert(gs.supply_count_pile(pile_name='curse') == 29)
	assert(gs.supply_count_pile(pile_name='trash') == 1)

def test_increment_player():
	gs = GameState(n_players = 3)
	assert(gs.current_player_index() == 0)
	assert(gs.prev_player_index() == 2)
	assert(gs.next_player_index() == 1)
	gs.increment_player()
	assert(gs.current_player_index() == 1)
	assert(gs.prev_player_index() == 0)
	assert(gs.next_player_index() == 2)
	

# Testing.

if __name__ == "__main__":
	print """play a limited version of dominion with 2 card types using GameState API."""
	my_gam
	e_state = GameState()
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
	print 'player 1 nVictory = ' + str(my_game_state._player_cards[0].count_in_all('Estate'))
	print 'player 2 nVictory = ' + str(my_game_state._player_cards[1].count_in_all('Estate'))
	if(my_game_state._player_cards[0].count_in_all('Estate') > my_game_state._player_cards[1].count_in_all('Estate')):
		print "Player 1 wins!!"
	else:
		print "Player 2 wins!!"






