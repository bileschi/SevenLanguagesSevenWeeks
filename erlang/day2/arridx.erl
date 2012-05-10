-module(arridx).
-export([arridx/2]).

% input L is a list like [{k1, v1}, {k2, v2}, ...]
% input K is k1, or k2 or something
% return the vx corresponding to kx.
arridx([{K, V} | _], K) -> V;
arridx([_ | T], K) -> arridx(T, K);
arridx([], _) -> no_match.

% DB = [{erlang, "a functional language"}, {ruby, "an OO language"}].
% arridx:arridx(DB, erlang).
% arridx:arridx(DB, ruby).
% arridx:arridx(DB, brainfuck).
