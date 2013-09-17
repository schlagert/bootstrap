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
%%% A simple example for a `gen_server' subscribing to `bootstrap'
%%% notifications. Compile with, e.g. `erlc -pa ../ebin/ example_server.erl'.
%%% @end
%%%=============================================================================

-module(example_server).

-behaviour(gen_server).
-behaviour(bootstrap).

%% API
-export([start_link/0]).

%% bootstrap callbacks
-export([on_connected/2,
         on_disconnected/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts an unregistered server, linked to the calling process.
%% @end
%%------------------------------------------------------------------------------
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%=============================================================================
%%% bootstrap callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
on_connected(Node, Pid) ->
    gen_server:cast(Pid, {connected, Node}),
    Pid.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
on_disconnected(Node, Reason, Pid) ->
    gen_server:cast(Pid, {disconnected, Node, Reason}),
    Pid.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    ok = bootstrap:add_sup_handler(?MODULE, self()),
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) -> {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({connected, Node}, State) ->
    error_logger:info_msg("Node ~s connected.", [Node]),
    {noreply, State};
handle_cast({disconnected, Node, Reason}, State) ->
    error_logger:info_msg("Node ~s disconnected (~p).", [Node, Reason]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.
