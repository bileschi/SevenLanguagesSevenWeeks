rev([], []).

rev([H|T],RevHT):- rev(T, RevT), append(RevT,[H],RevHT).
