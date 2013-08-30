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
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add(#bootstrap_handler{}) -> ok | {error, term()}.
add(Handler) -> gen_server:call(?MODULE, {add, Handler}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {
	  regex         :: re:mp(),
	  handlers = [] :: [{reference(), #bootstrap_handler{}}]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    case bootstrap:get_env(connect_to, undefined) of
	undefined ->
	    State = #state{regex = undefined};
	Regex when is_list(Regex) ->
	    {ok, Compiled} = re:compile(Regex),
	    State = #state{regex = Compiled}
    end,
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({add, Entry}, _From, State) ->
    {Reply, NewState} = handle_add(Entry, State),
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
handle_info({nodedown, Node, [{nodedown_reason, Reason}]}, State) ->
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
get_nodes(#state{regex = undefined}) ->
    [node()];
get_nodes(State) ->
    [Node || Node <- [node() | nodes()], match(Node, State)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_add(Handler = #bootstrap_handler{pid = undefined}, State) ->
    case bootstrap_event:add(Handler) of
	ok ->
	    bootstrap_event:on_connected(Handler, get_nodes(State)),
	    {ok, State};
	Reason ->
	    {{error, Reason}, State}
    end;
handle_add(Handler = #bootstrap_handler{pid = Pid}, State) ->
    case bootstrap_event:add(Handler) of
	ok ->
	    bootstrap_event:on_connected(Handler, get_nodes(State)),
	    Entry = {monitor(process, Pid), Handler},
	    {ok, State#state{handlers = [Entry | State#state.handlers]}};
	Reason ->
	    {{error, Reason}, State}
    end.

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
handle_nodeup(Node, State) ->
    case match(Node, State) of
	true  -> bootstrap_event:on_connected(Node);
	false -> ok
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_nodedown(Node, Reason, State) ->
    case match(Node, State) of
	true  -> bootstrap_event:on_disconnected(Node, Reason);
	false -> ok
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
match(_Node, #state{regex = undefined}) ->
    false;
match(Node, #state{regex = Regex}) ->
    re:run(atom_to_list(Node), Regex, [{capture, none}]) =:= match.
