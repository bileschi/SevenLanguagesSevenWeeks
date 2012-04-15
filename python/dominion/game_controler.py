#!/usr/bin/env python

from game_state import GameState
from player_transients import PlayerTransients
import terminal_view

class GameControler(object):
	"""
	Game Controler manages the dynamics and rules of the dominion game.  
	It connects to an instance of a game state, which it is responsible
	for twiddiling, according to the rules and input from the players.
	The players connect to the Game controler through the player_seats
	instance dict.  These are control points to which a player may connect / 
	disconnect.  Finally, the game controler also holds an instance of a 
	query interface.   This allows the game controler to respond to requsts
	regarding game state from the players.
	"""

	def __init__(self, game_state = None, n_players = 2):
		#'create a new game state if the input is None'
		self._game_state = game_state or GameState()
		#initialize player seats to all command line interface
		self._player_seats = []
		for seat in range(0,n_players):
			ps = {}
			ps['input'] = 'local_terminal_input'
			ps['output'] = 'local_terminal_output'
			sel._player_seats.append(ps)
		self._player_transients = PlayerTransients()

	def get_player_hand(self):
		return self._game_state.get_player_pile(pile_name='hand')

	def display_game_status(self):
		# get view for player.
		player = self._game_state.get_player()
		player_index = self._game_state.current_player_index();
		player_output = self._player_seats[player_index];
		if(player_output == "command_line"):
			print TerminalView.supply_pretty_str(game_state)
			print TerminalView.pretty_print_player_hand(game_state)

	def player_status_str(self):
		print  "current player index is:\t" + str(self._game_state.current_player_index())
		print "current phase:\t" + self._player_transients.phase_name()
		print "\tn_actions_left:\t" + str(self._player_transients.n_acts)
		print "\tn_buys_left:\t" + str(self._player_transients.n_buys)
		print "\tn_money:\t" + str(self._player_transients.n_money)

	def player_hand_str(self):
		self.get_player_hand().pretty_print()
		


	
	

if __name__=='__main__':
	print 'I am game controler'
	my_game_controler = GameControler()
	print my_game_controler
	print 'there are ' + str(len(my_game_controler._player_seats)) + 'players'
	print my_game_controler._game_state.list_supply_piles()
	print my_game_controler.display_game_status()
	my_game_controler.print_player_status()
	print '--- player hand ---'
	my_game_controler.print_player_hand()

