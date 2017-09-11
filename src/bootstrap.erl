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
%%% Main module of the `bootstrap' application.
%%%
%%% This module contains the API for bootstrap handler registration. Furthermore
%%% this module implements the application callback as well as the top level
%%% supervisor of the `bootstrap' application.
%%% @end
%%%=============================================================================
-module(bootstrap).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([monitor_nodes/1,
         handlers/0,
         deactivate/0,
         reactivate/0,
         info/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% supervisor callbacks
-export([init/1]).

-include("bootstrap.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Similar to {@link net_kernel:monitor_nodes/1}, signalling only mactching
%% nodes. The calling process subscribes or unsubscribes to node status change
%% messages. A nodeup message is delivered to all subscribing processes when a
%% new matching node is connected, and a nodedown message is delivered when a
%% matching node gets disconnected.
%%
%% In contrast to `net_kernel' nodeup messages for already connected matching
%% nodes will also be delivered to newly subscribing processes.
%%
%% If `Flag' is `true', a new subscription is started. If `Flag' is `false' a
%% previous subscription is stopped. There's no limit on the number of different
%% added handler processes. Subscribing an already subscribed process has no
%% effect. If a handler process exits it will automatically be unsubscribed.
%%
%% Message format is (also defined in bootstrap.hrl):
%% * `{bootstrap, {nodeup, Node :: atom()}}'
%% * `{bootstrap, {nodedown, Node :: atom(), Reason :: term()}}'
%%
%% @end
%%------------------------------------------------------------------------------
-spec monitor_nodes(boolean()) -> ok.
monitor_nodes(Flag) -> bootstrap_monitor:monitor_nodes(Flag, self()).

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all registered bootstrap handler processes.
%% @end
%%------------------------------------------------------------------------------
-spec handlers() -> [pid()].
handlers() -> bootstrap_monitor:handlers().

%%------------------------------------------------------------------------------
%% @doc
%% Deactivates bootstrap discovery on this node. This means that this node will
%% not (actively) take part in the discovery process any more. However, the
%% application will still be running and notifications for matching nodes will
%% still be delivered in case other active instances connect to this node
%% (passive mode). All network ports aquired by the application will be
%% released. Discovery can be restarted at any time using {@link reactivate/0}.
%% @end
%%------------------------------------------------------------------------------
-spec deactivate() -> ok | {error, term()}.
deactivate() -> supervisor:terminate_child(?MODULE, bootstrap_protocol).

%%------------------------------------------------------------------------------
%% @doc
%% Reactivates bootstrap discovery on this node. This only works when the
%% application has been deactivated before using {@link deactivate/0}.
%% @end
%%------------------------------------------------------------------------------
-spec reactivate() -> {ok, pid()} | {error, term()}.
reactivate() -> supervisor:restart_child(?MODULE, bootstrap_protocol).

%%------------------------------------------------------------------------------
%% @doc
%% Print information about the bootstrap instances on all connected nodes.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> ok.
info() ->
    %% TODO this should make use of rpc:multicall/X, but currently there's a
    %% problem which causes multicall to block forever when querying a java node
    [?INFO("~s:~n  Connections: ~w~n  Handlers:    ~w~n", [M, Cs, Hs])
     || N <- [node() | nodes(connected)],
        {ok, M, Cs, Hs} <- [rpc:call(N, bootstrap_lib, get_info, [], 1000)]],
    ok.

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
    Specs = [
             spec(bootstrap_monitor),
             spec(bootstrap_protocol)
            ],
    {ok, {{one_for_one, 0, 1}, Specs}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
spec(M) -> {M, {M, start_link, []}, permanent, brutal_kill, worker, [M]}.
