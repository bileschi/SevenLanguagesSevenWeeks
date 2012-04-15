class PlayerTransients(object):
	"""
	Player transients holds information about a player that is only useful during the duration
	of a turn.  State that persists after the end of a turn belongs in the GameState.
	"""
	turn_phases = ["action", "treasure", "buy"]

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

	def increment_phase(self):
		self.i_phase += 1;

	def player_is_done(self):
		return self.i_phase == len(self.turn_phases) - 1

########################################
############# Unit Tests ###############
########################################

def test_reset_new_turn():
	pt = PlayerTransients()
	assert(pt.n_buys == 1)
	assert(pt.n_acts == 1)
	assert(pt.n_money == 0)
	assert(pt.i_phase == 0)

def test_phase_name_access():
	pt = PlayerTransients()
	assert(pt.turn_phases[0] == "action")
	assert(pt.phase_name() == pt.turn_phases[0])

def test_increment_phase():
	pt = PlayerTransients()
	assert(not pt.player_is_done())
	pt.increment_phase()
	assert(not pt.player_is_done())
	pt.increment_phase()
	assert(pt.player_is_done())

