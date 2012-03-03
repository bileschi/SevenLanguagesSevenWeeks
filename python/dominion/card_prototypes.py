#!/usr/bin/env python

class card_prototype_data(object):
	def __init__(self, 
		name,
		is_treasure = False,
		is_action = False,
		is_victory = False,
		is_curse = False, 
		purchase_cost = 0,
		money_from_treasure = 0,
		acts_from_act = 0,
		buys_from_act = 0,
		cards_from_act = 0,
		money_from_act = 0,
		victory_points = 0):

		self.name = name
		self.purchase_cost = purchase_cost;
		# card types
		self.is_treasure = is_treasure;
		self.is_action = is_action;
		self.is_victory = is_victory;
		self.is_curse = is_curse;
		# card effects -- treasure phase
		self.money_from_treasure = money_from_treasure; # money gained from playing during treasure phase
		# card effects -- action phase
		self.acts_from_act = acts_from_act;   
		self.buys_from_act = buys_from_act;   
		self.cards_from_act = cards_from_act; 
		self.money_from_act = money_from_act; 
		# card effects -- victory phase
		self.victory_points = victory_points;
	def __str__(self):
		return self.name

card_prototypes = {}

card_prototypes['copper'] = card_prototype_data(
	name = 'copper',
	is_treasure = True,
	purchase_cost = 0,
	money_from_treasure = 1
	)

card_prototypes['silver'] = card_prototype_data(
	name = 'silver',
	is_treasure = True,
	purchase_cost = 3,
	money_from_treasure = 2
	)

card_prototypes['gold'] = card_prototype_data(
	name = 'gold',
	is_treasure = True,
	purchase_cost = 6,
	money_from_treasure = 3
	)

card_prototypes['estate'] = card_prototype_data(
	name = 'estate',
	is_victory = True,
	purchase_cost = 2,
	victory_points = 1
	)

card_prototypes['duchy'] = card_prototype_data(
	name = 'duchy',
	is_victory = True,
	purchase_cost = 5,
	victory_points = 3
	)

card_prototypes['province'] = card_prototype_data(
	name = 'province',
	is_victory = True,
	purchase_cost = 8,
	victory_points = 6
	)

card_prototypes['curse'] = card_prototype_data(
	name = 'curse',
	is_curse = True, 
	purchase_cost = 0,
	victory_points = -1
	)

card_prototypes['cellar'] = card_prototype_data(
	name = 'cellar',
	is_action = True,
	purchase_cost = 2,
	acts_from_act = 1
	)

card_prototypes['market'] = card_prototype_data(
	name = 'market',
	is_action = True,
	purchase_cost = 5,
	money_from_act = 1,
	acts_from_act = 1,
	cards_from_act = 1,
	buys_from_act = 1
	)

card_prototypes['militia'] = card_prototype_data(
	name = 'militia',
	is_action = True,
	purchase_cost = 4,
	money_from_act = 2
	)

card_prototypes['mine'] = card_prototype_data(
	name = 'mine',
	is_action = True,
	purchase_cost = 5
)

card_prototypes['moat'] = card_prototype_data(
	name = 'moat',
	is_action = True,
	purchase_cost = 2,
	cards_from_act = 2
)

card_prototypes['remodel'] = card_prototype_data(
	name = 'remodel',
	is_action = True,
	purchase_cost = 4
)

card_prototypes['smithy'] = card_prototype_data(
	name = 'smithy',
	is_action = True,
	purchase_cost = 4,
	cards_from_act = 3
)

card_prototypes['village'] = card_prototype_data(
	name = 'village',
	is_action = True,
	purchase_cost = 3,
	cards_from_act = 1,
	acts_from_act = 2
)

card_prototypes['woodcutter'] = card_prototype_data(
	name = 'woodcutter',
	is_action = True,
	purchase_cost = 3,
	buys_from_act = 1,
	money_from_act = 2
)

card_prototypes['workshop'] = card_prototype_data(
	name = 'workshop',
	is_action = True,
	purchase_cost = 3
)
