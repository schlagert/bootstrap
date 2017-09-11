%%%=============================================================================
%%% Copyright 2013-2017, Tobias Schlager <schlagert@github.com>
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
%%%
%%% @doc
%%% Common library functions used throughout the `bootstrap' application.
%%% @end
%%%=============================================================================
-module(bootstrap_lib).

%% API
-export([get_env/1,
         get_env/2,
         matches/2,
         pattern/0,
         mode/0,
         matching/2,
         nodes/1,
         get_info/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Retrieve a value from the application environment, crash if value is not set.
%% @end
%%------------------------------------------------------------------------------
-spec get_env(atom()) -> term() | undefined.
get_env(Key) -> element(2, {ok, _} = application:get_env(bootstrap, Key)).

%%------------------------------------------------------------------------------
%% @doc
%% Retrieve a value from the application environment, providing the default
%% value if not set.
%% @end
%%------------------------------------------------------------------------------
-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    case application:get_env(bootstrap, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Return whether a node name matches the given match pattern.
%% @end
%%------------------------------------------------------------------------------
-spec matches(node(), re:mp()) -> boolean().
matches(Node, Pattern) ->
    re:run(atom_to_list(Node), Pattern, [{capture, none}]) =:= match.

%%------------------------------------------------------------------------------
%% @doc
%% Return the compiled match pattern for `connect_regex' from the application
%% environment, crash if it is not set.
%% @end
%%------------------------------------------------------------------------------
-spec pattern() -> re:mp().
pattern() -> element(2, {ok, _} = re:compile(get_env(connect_regex))).

%%------------------------------------------------------------------------------
%% @doc
%% Return the configured `connect_mode' from the application environment, crash
%% if it is not set.
%% @end
%%------------------------------------------------------------------------------
-spec mode() -> visible | hidden.
mode() -> get_env(connect_mode).

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of connected matching nodes.
%% @end
%%------------------------------------------------------------------------------
-spec matching(re:mp(), visible | hidden) -> [node()].
matching(Pattern, Mode) -> [N || N <- ?MODULE:nodes(Mode), matches(N, Pattern)].

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all (not only matching) connected nodes for the given
%% `connect_mode'.
%% @end
%%------------------------------------------------------------------------------
-spec nodes(visible | hidden) -> [node()].
nodes(Mode) -> [node() | erlang:nodes(to_nodes_arg(Mode))].

%%------------------------------------------------------------------------------
%% @doc
%% Collect useful `bootstrap' specific information for this node.
%% @end
%%------------------------------------------------------------------------------
-spec get_info() -> {ok, node(), [node()], [module()]}.
get_info() -> {ok, node(), matching(pattern(), mode()), bootstrap:handlers()}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_nodes_arg(hidden)  -> connected;
to_nodes_arg(visible) -> visible.
