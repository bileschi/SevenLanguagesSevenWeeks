-module(errmsg).
-export([err/1]).

err(success) -> io:format("success!~n",[]), ok;
err({error, Msg}) -> io:format("error: ~s!~n",[Msg]), ok.