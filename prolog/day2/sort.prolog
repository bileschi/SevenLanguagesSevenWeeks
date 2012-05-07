% via: http://ktiml.mff.cuni.cz/~bartak/prolog/sorting.html

mysort([], []).
mysort([H], [H]).
mysort([H|T],Sorted):-
	pivoting(H,T,L1,L2),mysort(L1,Sorted1),mysort(L2,Sorted2),
	append(Sorted1,[H|Sorted2], Sorted).
   
pivoting(H,[],[],[]).
pivoting(H,[X|T],[X|L],G):-X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X>H,pivoting(H,T,L,G).




% faster version
%  using accumulator

quick_sort2(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).