#!/usr/bin/env python

"""
Game State holds everything you would need to know to package up an existing Dominion game and start it again later.
It does not hold anything about the logic of the game, or how players interact, that belongs elsewhere.
The game state holds, enumerated
1. The Supply:  The set of cards from which playsers may gain into their own hands / decks / discard 
2. The PlayerCards:  Each PlayerCards object holds the state of all of the items which belong to a player, including
which cards are in his deck, and in which order.  The cards on his discard, in his hand. etc.
3. History:  The sequence of previous actions 
4. Player Order: Which players turn is it and who will come next.  and then who.  and then who ...
"""

from cards import *
from collections import Counter

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
	def pretty_print(self):
		for pile_name, pile in self._piles.iteritems():
			print pile_name + ": " + str(pile.count_any_card())
	def put_in_trash(self, card):
		self._piles["Trash"].add_card_to_pile(card)


class PlayerCards(object):
	""" Defines the structured set of cards which belong to one player.  Includes, at minimum the player's hand,
	his deck, and his discard pile.  Provides some basic manipulations for the player, such as drawing cards, 
	shuffling back in his discard pile, and setting a starting deck."""
	def __init__(self):
		self._piles = None
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
		self._piles['deck'].add_others_to_pile(self._piles['discard'])
		self._piles['deck'].shuffle()
	def reset_empty(self):
		self._piles = {}
		self._piles['deck'] = Pile()
		self._piles['hand'] = Pile()
		self._piles['discard'] = Pile()
		self._piles['in_play'] = Pile()
	def reset_new_game(self):
		self._piles['deck'] = Pile('Copper',7)
		self._piles['deck'].add_new_card_to_pile('Estate',3)
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

class GameState(object):
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

################ Definition Complete ###########


# Testing.

if __name__ == "__main2__":

	### Testing create player cards and shuffle
	pc = PlayerCards()
	print pc
	print "BEFORE NEW GAME"
	pc.pretty_print()
	print "NEW GAME!"
	pc.reset_new_game()
	pc.pretty_print()
	print "Copper in hand " + str(pc.count_in_hand('Copper'))
	print "Estate in hand " + str(pc.count_in_hand('Estate'))

	### Testing count 5-2 vs 4-3 initializations
	nCopper = []
	nHands = 12345
	print "determing 5-2 ratio by simulating " + str(12345) + " hands"
	for iTest in range(0,nHands):
		pc.reset_new_game()
		nCopper.append(pc.count_in_hand('Copper'))
	countNCopperHands = Counter(nCopper)
	print str(countNCopperHands)
	print "portion of 5-2 starts (should be 0.166...) : " + str((countNCopperHands[5] + countNCopperHands[2]) / float(nHands))

	### Teting Supply
	supply = Supply()
	supply.set_pile('Copper', 30)
	supply.set_pile('Estate', 30)
	print "i see: " + str(supply.count_pile('Copper')) + " Copper"
	print "i see: " + str(supply.count_pile('Estate')) + " estates"
	for i in range(0, 32):
		print str(supply.take_one('Copper'))
	print "i see: " + str(supply.count_pile('Copper')) + " Copper"

	### Testing two players compete in a very simple game with a very simple controller
	print """A simple game:  only Copper and estates.  Estates cost 2.  Player 1 always buys an
	an Estate if he can.  Player 2 buys estates if he has 3 or more Copper."""
	pc1 = PlayerCards()
	pc2 = PlayerCards()
	pc1.reset_new_game()
	pc2.reset_new_game()
	supply = Supply()
	supply.set_pile('Copper', 30)
	supply.set_pile('Estate', 30)
	playerOrder = [pc1, pc2]
	nPlayers = len(playerOrder)
	playerNum = 0;
	thresh_to_buy_estate = [2,3]
	while supply.count_pile('Estate') > 0:
		print 'round: ' + str(playerNum)
		nextPlayerIndex = playerNum % nPlayers
		nextPlayer = playerOrder[nextPlayerIndex]
		money = nextPlayer.count_in_hand('Copper')
		print ' "I am player ' + str(nextPlayerIndex + 1)
		print ' "I have ' + str(money) + ' dollars."',
		if money >= thresh_to_buy_estate[nextPlayerIndex]:
			card = supply.take_one('Estate')
		else:
			card = supply.take_one('Copper')
		print ' "I buy a ' + str(card) + '."'
		nextPlayer.put_card_on_pile(card, 'discard')
		nextPlayer.put_pile_in_other_pile('hand','discard')
		nextPlayer.put_pile_in_other_pile('in_play','discard')
		nextPlayer.draw_n(5)
		playerNum += 1
	print 'player 1 nVictory = ' + str(pc1.count_in_all('Estate'))
	print 'player 2 nVictory = ' + str(pc2.count_in_all('Estate'))
	if(pc1.count_in_all('Estate') > pc2.count_in_all('Estate')):
		print "Player 1 wins!!"
	else:
		print "Player 2 wins!!"

if __name__ == "__main__":
	print """Repeat above game using gameState API."""
	gameState = GameState()
	print gameState.list_supply_piles()
	gameState._supply.set_pile('Estate', 30)
	gameState._supply.pretty_print()
	thresh_to_buy_estate = [2,3]
	while gameState._supply.count_pile('Estate') > 0:
		print 'Turn: ' + str(gameState._turn_num)
		player = gameState.get_player()
		money = player.count_in_hand('Copper')
		print ' "I am player ' + str(gameState.current_player_index() + 1)
		print ' "I have ' + str(money) + ' dollars."',
		if money >= thresh_to_buy_estate[gameState.current_player_index()]:
			gameState.player_gains_card_from_supply(supply_pile_name = "Estate")
			card = gameState._supply.take_one('Estate')
		else:
			gameState.player_gains_card_from_supply(supply_pile_name = "Copper")
			card = gameState._supply.take_one('Copper')
		print ' "I buy a ' + str(card) + '."'
		player.put_card_on_pile(card, 'discard')
		player.put_pile_in_other_pile('hand','discard')
		player.put_pile_in_other_pile('in_play','discard')
		player.draw_n(5)
		gameState.increment_player()
	print 'player 1 nVictory = ' + str(gameState._players[0].count_in_all('Estate'))
	print 'player 2 nVictory = ' + str(gameState._players[1].count_in_all('Estate'))
	if(gameState._players[0].count_in_all('Estate') > gameState._players[1].count_in_all('Estate')):
		print "Player 1 wins!!"
	else:
		print "Player 2 wins!!"






