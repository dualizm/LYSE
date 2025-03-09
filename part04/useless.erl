-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
-author("Dua").
-myown(aboba).

-define(sub(X,Y), X - Y).

add(A, B) ->
  A + B.

%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
  io:format("Hello, world!~n").

greet_and_add_two(X) ->
  hello(),
  add(X,?sub(10, 8)).
