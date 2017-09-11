%%%=============================================================================
%%% Copyright 2017, Tobias Schlager <schlagert@github.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%=============================================================================

-module(bootstrap_lib_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bootstrap/include/bootstrap.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

get_env_test_() ->
    {setup, setup(), teardown(),
     [
      fun get_env_1/0,
      fun get_env_2/0
     ]}.

get_env_1() ->
    ?assertEqual(value1, bootstrap_lib:get_env(key1)),
    ?assertEqual(value2, bootstrap_lib:get_env(key2)),
    ?assertError({badmatch, undefined}, bootstrap_lib:get_env(key3)).

get_env_2() ->
    ?assertEqual(value1, bootstrap_lib:get_env(key1, undefined)),
    ?assertEqual(value2, bootstrap_lib:get_env(key2, undefined)),
    ?assertEqual(undefined, bootstrap_lib:get_env(key3, undefined)).

matches_test() ->
    ?assert(bootstrap_lib:matches(node(), "")),
    ?assert(bootstrap_lib:matches(node(), ".*")),
    ?assert(bootstrap_lib:matches('joe@erlang.org', "joe@")),
    ?assert(not bootstrap_lib:matches('joe@erlang.org', "mike@")).

pattern_test() ->
    ok = application:set_env(bootstrap, connect_regex, "test"),
    Pattern = bootstrap_lib:pattern(),
    ?assert(bootstrap_lib:matches('test', Pattern)),
    ?assert(not bootstrap_lib:matches('bla', Pattern)).

mode_test() ->
    ok = application:set_env(bootstrap, connect_mode, da_mode),
    ?assertEqual(da_mode, bootstrap_lib:mode()).

nodes_test() ->
    ?assertEqual([node()], bootstrap_lib:nodes(visible)),
    ?assertEqual([node()], bootstrap_lib:nodes(hidden)).

matching_test() ->
    Pattern = atom_to_list(node()),
    ?assertEqual([node()], bootstrap_lib:matching(Pattern, visible)),
    ?assertEqual([node()], bootstrap_lib:matching(Pattern, hidden)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup() ->
    fun() ->
            ok = application:set_env(bootstrap, key1, value1),
            ok = application:set_env(bootstrap, key2, value2),
            ok = application:set_env(bootstrap, connect_regex, ".*")
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
teardown() -> fun(ok) -> ok end.
