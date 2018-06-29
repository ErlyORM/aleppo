-module(macro_string).

-export([f/1]).

-define(BAR(S, X), {S, X}).
-define(FOO(E), ?BAR(??E, Val)).

-spec f(any()) -> any().
f(Val) -> ?FOO(Val).
