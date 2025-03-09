-module(functions).
-export([greet/2, head/1, second/1, same/2, valid_time/1]).

greet(Gender, Name) ->
  io:format("Hello, ~s~s!", [gender_prefix(Gender), Name]).

gender_prefix(male) -> "Mr. ";
gender_prefix(female) -> "Mrs. ";
gender_prefix(_) -> "".

head([H | _]) -> H.

second([_, X | _]) -> X.

same(X, X) -> true;
same(_, _) -> false.

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p, ~n",
    [Date, Y, M, D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n",
    [Time, H, Min, S]);
valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").
