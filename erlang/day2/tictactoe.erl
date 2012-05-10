-module(tictactoe).
-export([win/1]).

% input is a list of nine elements representing a board like
% [1,2,3,4,5,6,7,8,9] represents
% 1 | 2 | 3 
% --+---+---
% 4 | 5 | 6 
% --+---+---
% 7 | 8 | 9 

% each element should be x, o, or s for space
% rows.
win([X,X,X,_,_,_,_,_,_]) when X /= s -> {win, X};
win([_,_,_,X,X,X,_,_,_]) when X /= s -> {win, X};
win([_,_,_,_,_,_,X,X,X]) when X /= s -> {win, X};
% columns.
win([X,_,_,X,_,_,X,_,_]) when X /= s -> {win, X};
win([_,X,_,_,X,_,_,X,_]) when X /= s -> {win, X};
win([_,_,X,_,_,X,_,_,X]) when X /= s -> {win, X};
% diagonals
win([X,_,_,_,X,_,_,_,X]) when X /= s -> {win, X};
win([_,_,X,_,X,_,X,_,_]) when X /= s -> {win, X};

win(Board) -> win(Board, lists:any(fun(X) -> X == s end, Board)).
win(_, true)  -> still_space;
win(_, false)  -> tie_game.

