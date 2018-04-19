-module(aleppo_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([nested_macro_string/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [nested_macro_string].
all() -> [nested_macro_string].

-spec nested_macro_string(config()) -> {comment, []}.
nested_macro_string(Config) ->
  DataDir = ?config(data_dir, Config),
  Path    = filename:join(DataDir, "macro_string.erl"),
  {ok, _} = aleppo:process_file(Path),

  {comment, []}.
