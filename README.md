Aleppo: ALternative Erlang Pre-ProcessOr
========================================

Aleppo is an alternative to epp(3erl), and is mainly intended for Erlang compiler hackers.

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
