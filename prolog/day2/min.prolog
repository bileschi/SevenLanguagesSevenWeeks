% via: http://stackoverflow.com/questions/3965054/prolog-find-minimum-in-a-list

min( [H], H).

min([H,K|T],M) :- H =< K, min([H|T],M). 
min([H,K|T],M) :- H > K,  min([K|T],M).


