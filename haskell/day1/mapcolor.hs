-- map coloring problem.  Assign colors to states such that no two adjacent states have the same color.

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