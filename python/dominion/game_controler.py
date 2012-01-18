#!/usr/bin/env python

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

