-- map coloring problem.  Assign colors to states such that no two adjacent states have the same color.
import Data.List

-- Tennesee
-- Mississippi
-- Alabama
-- Georgia
-- Florida 
states = [
 "Tennessee",
 "Mississippi",
 "Alabama",
 "Georgia",
 "Florida"]

colors = [
 "RED",
 "GREEN",
 "BLUE"]


-- adjacent states is not currently used.
adjacent_states = [
 ("Tennessee","Mississippi"),
 ("Tennessee","Alabama"),
 ("Tennessee","Georgia"),
 ("Mississippi","Alabama"),
 ("Alabama","Georgia"),
 ("Alabama","Florida"),
 ("Georgia","Florida")]

----------------- First approach -----------

-- valid colors still conflates logic and data.  I'd rather it depended on adgacent_states to determine the logic.
-- but this works :/
valid_colors c1 c2 c3 c4 c5 = c1 /= c2 && c1 /= c3 && c1 /= c4 && c2 /= c3 && c3 /= c4 && c3 /= c5 && c4 /= c5

-- assign map colors to states
-- solution as a list comprehension that produces a tuple of colors, corresponding to the ordered list of states.
-- probably not very efficient (ncolors ^ nstates possbile settings)
-- a better solution will use a decomposition of the problem resloving the into an assignment of a state to a color and
-- a smaller problem.
--
-- still conflates data and logic in that there are as many variables as states.  can that be fixed?
my_colors = [ [c1, c2, c3, c4, c5] |
	c1 <- colors, c2 <- colors, c3 <- colors, c4 <- colors, c5 <- colors,
	valid_colors c1 c2 c3 c4 c5]


color_states = zip states (head my_colors)

---------------- Trying a second approach.

-- l is a list, and constraints is a list of integer tuples
-- function returns true iff for each tuple (idx1, idx2) l[idx1] /= l[idx2]
-- this is implemented as a tail recursive function
check_elements_neq :: (Eq a) => [a] -> [(Int, Int)] -> Bool
check_elements_neq l [] = True -- no constraints returns true
 -- check the first constraint and recurse
check_elements_neq l ((idx1, idx2):rest) = ((l !! idx1) /= (l !! idx2)) && (check_elements_neq l rest)

-- builds integral constraints from the states and state-edges data
index_constraints :: (Eq a) => [a] -> [(a,a)] -> [(Maybe Int, Maybe Int)]
index_constraints v [] = []
index_constraints v ((n1, n2):rest) = ((elemIndex n1 v), (elemIndex n2 v)) : (index_constraints v rest)

-- removes data errors stemming from state-borders to undefined states
filter_constraints :: [(Maybe t0, Maybe t1)] -> [(t0, t1)]
filter_constraints [] = []
filter_constraints ((Nothing, _):rest) = filter_constraints rest
filter_constraints ((_, Nothing):rest) = filter_constraints rest
filter_constraints ((Just a, Just b):rest) = (a,b):(filter_constraints rest)

-- my_colors' still only works for sets of 5 states.  this needs to generalize to arbitrary lists of states
my_colors' = [ [c1, c2, c3, c4, c5] |
	c1 <- colors, c2 <- colors, c3 <- colors, c4 <- colors, c5 <- colors,
	check_elements_neq [c1, c2, c3, c4, c5] (filter_constraints (index_constraints states adjacent_states))]

color_states' = zip states (head my_colors')



------------ Third approach, solve via tail recursion leveraging partial solutions

-- this is the top level call.  
-- You give it 
-- 1. a list of states to color (they must be unique), 
-- 2. a list of colors to use,
-- 3. a list of pairs of states, representing state adjacency.
-- it will return a [[(state, color)]] representing a list of maps from states to colors satisfiying all constraints.  
color_states'' :: (Eq t_state, Eq t_color) => [t_state] -> [t_color] -> [(t_state, t_state)] -> [[(t_state, t_color)]]
color_states'' [] _  _ = []
color_states'' _  [] _= []
color_states'' states colors adjacency = color_state_helper states colors adjacency [] -- helper passes partial solutions


-- used by color_states''
-- You give it 
-- 1. a list of states to color (they must be unique), 
-- 2. a list of colors to use,
-- 3. a list of pairs of states, representing state adjacency.
-- 4. a partial solution (can be [] )
-- it will return a [[(state, color)]] representing a list of maps from states to colors satisfiying all constraints.  
color_state_helper :: (Eq t_state, Eq t_color) => [t_state] -> [t_color] -> [(t_state, t_state)] -> [[(t_state, t_color)]] -> [[(t_state, t_color)]]
color_state_helper [] _ _ _ = [[]] -- no states
color_state_helper _ [] _ _	 = [[]] -- no colors
color_state_helper (s:rest_states) colors adjacency [] 
	| rest_states == [] = [[(s,c)] | c <- colors]
	| otherwise = color_state_helper  (s:rest_states) colors adjacency ( color_state_helper rest_states colors adjacency [])
color_state_helper (s:rest_states) colors adjacency prt_solns = -- given a set of solutions to every state but this_state:
 	[ (s,c):prt_soln | prt_soln <- prt_solns,					-- result is like (this_state, some_color):partial_solution 
	c <- avail_colors s colors prt_soln adjacency]				-- where some_color is in colors but is not already used by some
																-- state which neighbors this_state

-- gives colors from list of colors that can be assigned to first argument state without assigning the same
-- color to a state adjacent to the first argument state
avail_colors :: (Eq t_color, Eq t_state) => t_state -> [t_color] -> [(t_state, t_color)] -> [(t_state, t_state)] -> [(t_color)]
avail_colors state colors prt_soln adjacency = [c | c <- colors , not (elem c (neighbor_colors state prt_soln adjacency))]

-- returns the list of colors of states adacent to the first argument state.
-- may contain duplicates.
neighbor_colors :: (Eq t_state, Eq t_color) => t_state -> [(t_state, t_color)] -> [(t_state, t_state)] -> [(t_color)]
neighbor_colors _ [] _ = []
neighbor_colors _ _ [] = []
neighbor_colors s prt_soln adjacency = [snd sc | sc <- prt_soln, or [(elem (fst sc, s) adjacency), (elem (s, fst sc) adjacency) ]]


-- ok finally, the third version is completely general and data driven.
--color_states'' states colors adjacent_states


------------- Bigger tests

more_states = [
 "Louisiana",
 "Arkansas",
 "Tennessee",
 "Mississippi",
 "Alabama",
 "Georgia",
 "Florida"]

more_colors = [
 "RED",
 "GREEN",
 "BLUE",
 "LIGHT URPLE"
 ]


-- adjacent states is not currently used.
more_adjacent_states = [
 ("Louisiana", "Arkansas"),
 ("Louisiana", "Mississippis"),
 ("Arkansas", "Tennessee"),
 ("Arkansas", "Mississippi"),
 ("Tennessee","Mississippi"),
 ("Tennessee","Alabama"),
 ("Tennessee","Georgia"),
 ("Mississippi","Alabama"),
 ("Alabama","Georgia"),
 ("Alabama","Florida"),
 ("Georgia","Florida")]

--color_states'' more_states colors more_adjacent_states
