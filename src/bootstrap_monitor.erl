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
%%% A server that monitors bootstrap handler registrations as well as connection
%%% state to remote nodes.
%%% @end
%%%=============================================================================
-module(bootstrap_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add/1]).

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
%% Add a bootstrap handler to the {@link bootstrap_event}. If the passed handler
%% structure contains a process id, it will be monitored and the corresponding
%% handler will be removed when the process exits. If the given module is
%% already registered, adding the handler will fail.
%% @end
%%------------------------------------------------------------------------------
-spec add(#bootstrap_handler{}) -> ok | {error, term()}.
add(Handler) -> gen_server:call(?MODULE, {add, Handler}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {
          pattern       :: re:mp(),
          mode          :: visible | hidden,
          handlers = [] :: [{reference(), #bootstrap_handler{}}]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    Hs = bootstrap:get_env(handlers, []),
    ok = bootstrap:set_env(handlers, []),
    Mode = bootstrap:get_env(connect_mode, ?CONNECT_MODE),
    TypeOpt = {node_type, case Mode of hidden -> all; visible -> Mode end},
    ok = net_kernel:monitor_nodes(true, [TypeOpt, nodedown_reason]),
    {ok, lists:foldl(
           fun(H, S) -> element(2, handle_add(false, H, S)) end,
           #state{pattern = bootstrap:pattern(), mode = Mode}, Hs)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({add, Entry}, _From, State) ->
    {Reply, NewState} = handle_add(true, Entry, State),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(_Request, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({gen_event_EXIT, ?BOOTSTRAP_HANDLER(Module), _}, State) ->
    {noreply, handle_exit(Module, State)};
handle_info({'DOWN', Ref, process, _, _}, State) ->
    {noreply, handle_down(Ref, State)};
handle_info({nodeup, Node, _}, State) ->
    {noreply, handle_nodeup(Node, State)};
handle_info({nodedown, Node, List}, State) ->
    Reason = proplists:get_value(nodedown_reason, List),
    {noreply, handle_nodedown(Node, Reason, State)};
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
matching(#state{pattern = Pattern}) -> bootstrap:matching(Pattern).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_add(Report, Handler = #bootstrap_handler{pid = Pid}, State) ->
    case bootstrap_event:add(Handler) of
        ok when is_pid(Pid) ->
            NewHs = [{monitor(process, Pid), Handler} | State#state.handlers],
            {ok, maybe_report(Report, Handler, State#state{handlers = NewHs})};
        ok ->
            {ok, maybe_report(Report, Handler, State)};
        Reason ->
            {{error, Reason}, State}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_report(true, Handler, State) ->
    %% Do not block the whole server, on_connected/2 is synchronous.
    spawn(fun() -> bootstrap_event:on_connected(Handler, matching(State)) end),
    State;
maybe_report(false, _Handler, State) ->
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_exit(Module, State = #state{handlers = Hs}) ->
    Fun = fun({_, #bootstrap_handler{module = M}}) -> M =:= Module end,
    {DelHs, NewHs} = lists:partition(Fun, Hs),
    [demonitor(Ref) || {Ref, _} <- DelHs],
    State#state{handlers = NewHs}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_down(Ref, State = #state{handlers = Hs}) ->
    {DelHs, NewHs} = lists:partition(fun({R, _}) -> R =:= Ref end, Hs),
    [bootstrap_event:delete(H#bootstrap_handler.module) || {_, H} <- DelHs],
    State#state{handlers = NewHs}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_nodeup(Node, State = #state{pattern = Pattern, mode = Mode}) ->
    case bootstrap:matches(Node, Pattern) of
        true  -> report(Node, Mode);
        false -> ok
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_nodedown(Node, Reason, State = #state{pattern = Pattern}) ->
    case bootstrap:matches(Node, Pattern) of
        true  -> bootstrap_event:on_disconnected(Node, Reason);
        false -> ok
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
report(Node, visible) ->
    %% Do not block the whole server, since sync may take a while.
    spawn(fun() ->
                  Time = element(1, timer:tc(global, sync, [])) div 1000,
                  ?DBG("Global synchronization took ~pms.~n", [Time]),
                  bootstrap_event:on_connected(Node)
          end);
report(Node, hidden) ->
    bootstrap_event:on_connected(Node).
