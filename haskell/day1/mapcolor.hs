-- map coloring problem.  Assign colors to states such that no two adjacent states have the same color.
import Data.List

-- Tennesee
-- Mississippi
-- Alabama
-- Georgia
-- Florida 
states = [
 "Tennesee",
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
 ("Tennesee","Mississippi"),
 ("Tennesee","Alabama"),
 ("Tennesee","Georgia"),
 ("Mississippi","Alabama"),
 ("Alabama","Georgia"),
 ("Alabama","Florida"),
 ("Georgia","Florida")]

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

my_colors' = [ [c1, c2, c3, c4, c5] |
	c1 <- colors, c2 <- colors, c3 <- colors, c4 <- colors, c5 <- colors,
	check_elements_neq [c1, c2, c3, c4, c5] (filter_constraints (index_constraints states adjacent_states))]


color_states' = zip states (head my_colors')














