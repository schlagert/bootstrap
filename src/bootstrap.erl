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
-export([add_handler/2, delete_handler/1, handlers/0]).

%% Internal API
-export([set_env/2, get_env/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback on_connected(node(), State :: term()) ->
    NewState :: term().
%% Called whenever a connection to a node matching the `master' regex has been
%% established. This may occur multiple times. The returned term will be the new
%% handler state. If the handler fails (throws an expection) it will be removed.

-callback on_disconnected(node(), Reason :: term(), State :: term()) ->
    NewState :: term().
%% Called whenever a connection to a node matching the `master' regex has been
%% lost. This may also occur multiple times. The returned term will be the new
%% handler state. If the handler fails (throws an expection) it will be removed.

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
%% @end
%%------------------------------------------------------------------------------
-spec add_handler(module(), State :: term()) -> ok | {error, term()}.
add_handler(Module, State) -> bootstrap_handlers:add(Module, State).

%%------------------------------------------------------------------------------
%% @doc
%% Deletes a bootstrap handler module. Calling this function with a module that
%% was not added before is not considered an error.
%% @end
%%------------------------------------------------------------------------------
-spec delete_handler(module()) -> ok.
delete_handler(Module) -> bootstrap_handlers:delete(Module).

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all registered bootstrap handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handlers() -> [module()].
handlers() -> [Module || {Module, _} <- bootstrap_handlers:to_list()].

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

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) -> supervisor:start_link(?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> set_env(handlers, bootstrap_handlers:to_list()).

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    ok = bootstrap_handlers:init(get_env(handlers, [])),
    {ok, {{one_for_one, 5, 10}, [spec(bootstrap_protocol)]}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
spec(M) -> {M, {M, start_link, []}, permanent, brutal_kill, worker, [M]}.
