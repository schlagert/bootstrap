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
%%% A server that monitors bootstrap handlers as well as connection state to
%%% remote nodes.
%%% @end
%%%=============================================================================
-module(bootstrap_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
         monitor_nodes/2,
         handlers/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("bootstrap.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts a locally registered generic server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% Add or delete a node monitoring process.
%% @end
%%------------------------------------------------------------------------------
-spec monitor_nodes(boolean(), pid()) -> ok.
monitor_nodes(true, Pid)  -> gen_server:cast(?MODULE, {add, Pid});
monitor_nodes(false, Pid) -> gen_server:cast(?MODULE, {delete, Pid}).

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of current handler pids.
%% @end
%%------------------------------------------------------------------------------
-spec handlers() -> [pid()].
handlers() -> gen_server:call(?MODULE, handlers).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {
          nodes = []    :: [node()],
          pattern       :: re:mp(),
          mode          :: visible | hidden,
          handlers = [] :: [{pid(), reference()}]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    Mode = bootstrap_lib:mode(),
    [self() ! {nodeup, Node, ignore} || Node <- bootstrap_lib:nodes(Mode)],
    MonitorOptions = to_monitor_nodes_options(Mode),
    ok = net_kernel:monitor_nodes(true, MonitorOptions),
    {ok, #state{pattern = bootstrap_lib:pattern(), mode = Mode}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(handlers, _From, State) -> {reply, get_handlers(State), State};
handle_call(_Request, _From, State) -> {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({add, Pid}, State)    -> {noreply, add_handler(Pid, State)};
handle_cast({delete, Pid}, State) -> {noreply, delete_handler(Pid, State)};
handle_cast(_Request, State)      -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({'DOWN', _, process, Pid, _}, State) ->
    {noreply, delete_handler(Pid, State)};
handle_info({nodeup, Node, _}, State) ->
    {noreply, report_new(matches(Node, State), Node, State)};
handle_info({nodedown, Node, List}, State) ->
    Reason = proplists:get_value(nodedown_reason, List),
    {noreply, report_down(matches(Node, State), Node, Reason, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_handlers(#state{handlers = Hs}) -> [Pid || {Pid, _Ref} <- Hs].

%%------------------------------------------------------------------------------
%% @private
%% Add a new handler pid, monitor if not yet added.
%%------------------------------------------------------------------------------
add_handler(Pid, State = #state{handlers = Hs}) ->
    case lists:keyfind(Pid, 1, Hs) of
        false ->
            Ref = erlang:monitor(process, Pid),
            report_current(Pid, State#state{handlers = [{Pid, Ref} | Hs]});
        _ ->
            State
    end.

%%------------------------------------------------------------------------------
%% @private
%% Remove/demonitor a handler pid.
%%------------------------------------------------------------------------------
delete_handler(Pid, State = #state{handlers = Hs}) ->
    case lists:keyfind(Pid, 1, Hs) of
        {Pid, Ref} ->
            true = erlang:demonitor(Ref),
            State#state{handlers = Hs -- [{Pid, Ref}]};
        false ->
            State
    end.

%%------------------------------------------------------------------------------
%% @private
%% Report all nodeup of all known matching nodes to a new handler.
%%------------------------------------------------------------------------------
report_current(Handler, State = #state{mode = Mode, nodes = Nodes}) ->
    [report([Handler], Mode, Node, up) || Node <- Nodes],
    State.

%%------------------------------------------------------------------------------
%% @private
%% Report a new node to all handlers if it matches.
%%------------------------------------------------------------------------------
report_new(true, Node, State = #state{mode = Mode}) ->
    report(get_handlers(State), Mode, Node, up),
    State#state{nodes = lists:usort([Node | State#state.nodes])};
report_new(false, _Node, State) ->
    State.

%%------------------------------------------------------------------------------
%% @private
%% Report a new node to all handlers if it matches.
%%------------------------------------------------------------------------------
report_down(true, Node, Reason, State = #state{mode = Mode}) ->
    report(get_handlers(State), Mode, Node, {down, Reason}),
    State#state{nodes = lists:usort(State#state.nodes -- [Node])};
report_down(false, _Node, _Reason, State) ->
    State.

%%------------------------------------------------------------------------------
%% @private
%% Report a node as up/down to a certain handler.
%%------------------------------------------------------------------------------
report(Handlers, visible, Node, up) ->
    %% This may block the whole server for some time. However, for the sake of
    %% correctness, we still must do it this way, because the last thing we
    %% want is disordered up/down messages from a node.
    case bootstrap_lib:get_env(no_global_sync, false) of
        false ->
            Time = element(1, timer:tc(global, sync, [])) div 1000,
            ?INFO("Global synchronization took ~pms.~n", [Time]);
        true ->
            ok
    end,
    [Handler ! ?BOOTSTRAP_UP(Node) || Handler <- Handlers];
report(Handlers, hidden, Node, up) ->
    [Handler ! ?BOOTSTRAP_UP(Node) || Handler <- Handlers];
report(Handlers, _, Node, {down, Reason}) ->
    [Handler ! ?BOOTSTRAP_DOWN(Node, Reason) || Handler <- Handlers].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
matches(Node, #state{pattern = Pattern}) ->
    bootstrap_lib:matches(Node, Pattern).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_monitor_nodes_options(visible) -> [{node_type, visible}, nodedown_reason];
to_monitor_nodes_options(hidden)  -> [{node_type, all}, nodedown_reason].
