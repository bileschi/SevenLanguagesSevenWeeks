-module(total).
-export([price/1]).

price(Cart) -> [ {Item, PerUnitCost * Count} || 
                 {Item, PerUnitCost , Count} <- Cart].

% Cart = [{bread, 2, 0.25}, {meat, 1, 1.25}, {pickle, 3, 0.10}].
% Tot = total:price(Cart).
