min_of_list_2( [H], H).
min_of_list_2( [H| T], X) :- min_of_list_2(T, X), compare(<, X, H).
min_of_list_2( [H| T], H) :- min_of_list_2(T, X), compare(>, X, H).
min_of_list_2( [H| T], H) :- min_of_list_2(T, X), compare(=, X, H).