%%%=============================================================================
%%% Copyright 2013, Tobias Schlager <schlagert@github.com>
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
%%% Main module of the `bootstrap' application.
%%%
%%% This module contains the API for bootstrap handler registration as well as
%%% the `bootstrap' behaviour definition that must be implemented by handlers.
%%%
%%% Furthermore this module implements the application callback as well as the
%%% top level supervisor of the `bootstrap' application.
%%% @end
%%%=============================================================================
-module(bootstrap).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([add_handler/2,
         add_sup_handler/2,
         delete_handler/1,
         handlers/0]).

%% Internal API
-export([set_env/2,
         get_env/2,
         matches/2,
         pattern/0,
         matching/1]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% supervisor callbacks
-export([init/1]).

-type arg() :: term().

-export_type([arg/0]).

-include("bootstrap.hrl").

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback on_connected(node(), arg()) -> arg().
%% Called whenever a connection to a node matching the `connect_regex' has been
%% established. This may occur multiple times. The returned term will be the
% new handler state. Exceptions thrown by this function will be discarded.

-callback on_disconnected(node(), Reason :: term(), arg()) -> arg().
%% Called whenever a connection to a node matching the `connect_regex' has been
%% lost. This may also occur multiple times. The returned term will be the new
%% handler state. Exceptions thrown by this function will be discarded.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Adds a new bootstrap handler module. The second argument is the initial
%% handler state passed into the behaviour callbacks. A specific module can only
%% be added once per time. If the same module was already added before it must
%% be deleted in advance. There's no limit on the number of different added
%% handler modules. Callback processing is sequential and in order of addition.
%% To remove a bootstrap handler {@link delete_handler/1} must be called.
%% @end
%%------------------------------------------------------------------------------
-spec add_handler(module(), arg()) -> ok | {error, term()}.
add_handler(Module, Arg) ->
    Handler = #bootstrap_handler{module = Module, arg = Arg},
    bootstrap_monitor:add(Handler).

%%------------------------------------------------------------------------------
%% @doc
%% This function works much like {@link add_handler/2} but additionally the
%% calling process will be associated with the bootstrap handler. This means
%% that the calling process will be monitored and the corresponding handler
%% will be removed as soon as the associated process exits. Alternatively the
%% handler can be removed manually using {@link delete_handler/1}.
%% @end
%%------------------------------------------------------------------------------
-spec add_sup_handler(module(), arg()) -> ok | {error, term()}.
add_sup_handler(Module, Arg) ->
    Handler = #bootstrap_handler{pid = self(), module = Module, arg = Arg},
    bootstrap_monitor:add(Handler).

%%------------------------------------------------------------------------------
%% @doc
%% Deletes a bootstrap handler module. Calling this function with a module that
%% was not added before is not considered an error.
%% @end
%%------------------------------------------------------------------------------
-spec delete_handler(module()) -> ok.
delete_handler(Module) ->
    bootstrap_event:delete(Module),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all registered bootstrap handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handlers() -> [module()].
handlers() -> bootstrap_event:list().

%%%=============================================================================
%%% Internal API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec set_env(atom(), term()) -> ok | {error, term()}.
set_env(Key, Value) -> application:set_env(?MODULE, Key, Value).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec matches(node(), re:mp()) -> boolean().
matches(Node, Pattern) ->
    re:run(atom_to_list(Node), Pattern, [{capture, none}]) =:= match.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec pattern() -> re:mp().
pattern() -> element(2, {ok, _} = re:compile(get_env(connect_regex, ".*"))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec matching(re:mp()) -> [node()].
matching(Pattern) ->
    [Node || Node <- [node() | nodes(connected)], matches(Node, Pattern)].

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case supervisor:start_link(?MODULE, []) of
        Result = {ok, _} ->
            [bootstrap_monitor:add(H) || H <- get_env(handlers, [])],
            ok = set_env(handlers, []),
            Result;
        Result ->
            Result
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> ok.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [event_mgr(bootstrap_event),
           server(bootstrap_monitor),
           server(bootstrap_protocol)]}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
server(M) -> spec(M, [M]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
event_mgr(M) -> spec(M, dynamic).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
spec(M, Ms) -> {M, {M, start_link, []}, permanent, brutal_kill, worker, Ms}.
