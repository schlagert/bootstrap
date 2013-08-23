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
-module(bootstrap_protocol).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,
         handle_sync_event/4,
         handle_event/3,
         handle_info/3,
         code_change/4,
         terminate/3]).

%% gen_fsm states
-export([master/2,
         slave/2]).

%%%=============================================================================
%%% Protocol
%%%=============================================================================

-define(bootstrap_ping(Node), {bootstrap, {ping, Node}}).
-define(bootstrap_pong(Node), {bootstrap, {pong, Node}}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_fsm:start_link(?MODULE, [], []).

%%%=============================================================================
%%% gen_fsm callbacks
%%%=============================================================================

-record(state, {
          master :: re:mp(),
          socket :: gen_udp:socket(),
          ports  :: [inet:port_number()]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    {ok, Master} = re:compile(get_env(master, atom_to_list(node()))),
    Ports = get_env(ports, [50337, 50338, 50339]),
    {ok, Socket} = open(Ports),
    State = #state{master = Master, socket = Socket, ports = Ports},
    {ok, case match(node(), State) of true -> master; _ -> slave end, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_sync_event(_Evt, _From, StateName, State) ->
    {reply, undef, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(_Evt, StateName, State) -> {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp, S, IP, Port, Data}, StateName, State = #state{socket = S}) ->
    try binary_to_term(Data) of
        Term -> ?MODULE:StateName({IP, Port, Term}, State)
    catch
        _:_ -> {next_state, StateName, State}
    end;
handle_info({udp_closed, S}, _StateName, State = #state{socket = S}) ->
    {stop, udp_closed, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket = Socket}) ->
    gen_udp:close(Socket).

%%%=============================================================================
%%% gen_fsm states
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
slave({_, _, ?bootstrap_pong(Node)}, State) ->
    {next_state, slave, maybe_connect(Node, State)};
slave(_Evt, State) ->
    {next_state, slave, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
master({IP, Port, ?bootstrap_ping(_)}, State) ->
    {next_state, master, send(?bootstrap_pong(node()), IP, Port, State)};
master(_Evt, State) ->
    {next_state, master, State}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
match(Subject, State) when is_atom(Subject) ->
    match(atom_to_list(Subject), State);
match(Subject, #state{master = MasterRegex}) ->
    re:run(Subject, MasterRegex, [{capture, none}]) =:= match.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
open(Ports)           -> lists:foldl(fun open/2, {error, no_ports}, Ports).
open(_, {ok, Socket}) -> {ok, Socket};
open(Port, _)         -> gen_udp:open(Port, [binary, {broadcast, true}]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_env(Key, Default) ->
    case application:get_env(bootstrap, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.
