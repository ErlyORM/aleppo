Aleppo: ALternative Erlang Pre-ProcessOr
========================================

Aleppo is an alternative to `epp(3erl)`, and is mainly intended for Erlang compiler hackers. The problem with EPP is that it only operates on Erlang source files. Aleppo will operate directly on tokens returned by erl_scan.

    erl_scan -> *aleppo* -> erl_parse -> compile

Features
--------

* Token-oriented API. Pass in tokens returned by erl_scan, receive a list of
  erl_scan-compatible tokens after preprocessor macros have been applied.
  No parsing of Erlang code is performed!

* Support for -define, -undef, -ifdef, -ifndef, -else, -endif, -include, -include_lib

* Preprocessor directives can be used inside functions


API
---

    aleppo:process_file(FileName) -> {ok, Tokens} | {error, Error}

    aleppo:process_tokens(Tokens) -> {ok, NewTokens} | {error, Error}

    aleppo:process_tokens(Tokens, Options) -> {ok, NewTokens} | {error, Error}

        Options = [{file, FileName}, {include, [IncludeSearchDirectories]}]


Notes
-----

When calling erl_scan:tokens/3, be sure to pass in a `{Row, Column}` tuple as the third argument (not just a Row integer). Otherwise Aleppo will not properly handle `-include` directives.
