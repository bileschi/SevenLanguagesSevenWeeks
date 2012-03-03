#!/usr/bin/env python

from game_state import GameState
import terminal_view

class PlayerTransients(object):
	"""
	Player transients holds information about a player that is only useful during the duration
	of a turn.  State that persists after the end of a turn belongs in the GameState.
	"""
	turn_phases = ["Action", "Treasure", "Buy"]
	def __init__(self):
		self.reset_new_turn()
	def reset_new_turn(self):
		self.n_buys = 1;
		self.n_acts = 1;
		self.n_money = 0;
		self.i_phase = 0;
	def phase_name(self):
		try:
			return self.turn_phases[self.i_phase]
		except IndexError, e:
			return "Done"
	def player_is_done(self):
		return self.i_phase == len(turn_phases)


class PlayerSeat(object):
	"""
	Controls input and output for a given player.  
	A player seat defines where the controler should turn to hear player moves 
	or display player related information.
	"""
	def __init__(self, player_input = None, player_output = None):
		self.player_input = player_input;
		self.player_output = player_output;


class GameControler(object):
	"""
	Game Controler manages the dynamics and rules of the dominion game.  
	It connects to an instance of a game state, which it is responsible
	for twiddiling, according to the rules, and input from the players.
	The players connect to the Game controler through instances of
	PlayerSeats.  These are control points to which a player my connect / 
	disconnect.  Finally, the game controler also holds an instance of a 
	query interface.   This allows the game controler to respond to requsts
	regarding game state from the players.
	"""
	def __init__(self, game_state = None, n_players = 2):
		#'create a new game state if the input is None'
		self._game_state = game_state or GameState()
		#initialize player seats to all command line interface
		self._player_seats = {}
		for seat in range(0,n_players):
			self._player_seats[seat] = PlayerSeat("terminal", "terminal")
		self._player_transients = PlayerTransients()
	def get_player_hand(self):
		return self._game_state.get_player_pile(pile_name='hand')
	def display_game_status(self):
		# get view for player.
		player = self._game_state.get_player()
		player_index = self._game_state.current_player_index();
		player_output = self._player_seats[player_index];
		if(player_output == "command_line"):
			TerminalView.pretty_print_supply(game_state)
			TerminalView.pretty_player_hand(game_state)

	def print_player_status(self):
		print  "current player index is:\t" + str(self._game_state.current_player_index())
		print "current phase:\t" + self._player_transients.phase_name()
		print "\tn_actions_left:\t" + str(self._player_transients.n_acts)
		print  "\tn_buys_left:\t" + str(self._player_transients.n_buys)
		print "\tn_money:\t" + str(self._player_transients.n_money)
	def print_player_hand(self):
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

