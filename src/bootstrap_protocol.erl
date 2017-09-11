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
%%% An unregistered server implementing the UDP based discovery protocol.
%%% This protocol basically consists of two PDUs that are represented by the
%%% following erlang terms encoded in the external binary format:
%%% * `{bootstrap, {ping, PingNode, PingAddr}}':
%%%   This message is issued to find other nodes. The sending nodes node name is
%%%   included as well as the address this request was issued over. This allows
%%%   easy answering as well as separating own packets.
%%% * `{bootstrap, {pong, Node, PingNode}}':
%%%   This message is the answer to a ping packet and contains the node name of
%%%   the answering node as well as the name of the node that sent the ping
%%%   this pong is an answer for. It is issued to the port the request was sent
%%%   from as well as to the `primary_port' (if different).
%%%
%%% To learn more on how discovery works, refer to the project's `README' file.
%%% @see bootstrap_broadcast
%%% @see bootstrap_multicast
%%% @end
%%%=============================================================================
-module(bootstrap_protocol).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("bootstrap.hrl").

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback options() -> [gen_udp:option()].
%% Called when the protocol socket gets initialized. The returned options will
%% be appended to the common options. E.g. for a broadcast UDP implementation,
%% the returned options should turn on broadcast support on a socket.

-callback addresses() -> [inet:ip4_address()].
%% Must return a list of addresses ping packets will be sent to. E.g. for a
%% broadcast implementation, this should return a list of IPv4 broadcast
%% addresses. This will be called frequently, whenever a node decides to ping
%% for other nodes.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts an unregistered generic server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {
          mode     :: visible | hidden,
          pattern  :: re:mp(),
          protocol :: module(),
          port     :: -1 | inet:port_number(),
          socket   :: inet:socket() | undefined,
          timer    :: reference() | undefined,
          minimum  :: non_neg_integer() | infinity,
          timeout  :: non_neg_integer()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, timer_backoff(
           open_udp_port(
             #state{
                mode     = bootstrap_lib:mode(),
                pattern  = bootstrap_lib:pattern(),
                protocol = to_mod(bootstrap_lib:get_env(protocol)),
                port     = bootstrap_lib:get_env(primary_port),
                minimum  = bootstrap_lib:get_env(min_connections),
                timeout  = bootstrap_lib:get_env(ping_timeout)}))}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) -> {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(_Request, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp, S, IP, Port, Data}, State = #state{socket = S}) ->
    try {IP, binary_to_term(Data)} of
        {_, ?BOOTSTRAP_PING(PingNode, _)} when PingNode == node() ->
            {noreply, State};
        {_, ?BOOTSTRAP_PING(PingNode, PingAddr)} ->
            ?DBG("Got PING from ~s with source port ~w.~n", [PingNode, Port]),
            {noreply, handle_ping(PingNode, PingAddr, Port, State)};
        {_, ?BOOTSTRAP_PONG(Node, _)} when Node == node() ->
            {noreply, State};
        {_, ?BOOTSTRAP_PONG(Node, PingNode)} ->
            ?DBG("Got PONG from ~s (answering ~s) with source port ~w.~n",
                 [Node, PingNode, Port]),
            {noreply, handle_pong(Node, PingNode, State)};
        {{I1, I2, I3, I4}, Msg} ->
            ?DBG("Ignoring ~w from ~w.~w.~w.~w:~w.~n",
                 [Msg, I1, I2, I3, I4, Port]),
            {noreply, State}
    catch
        _:_ -> {noreply, State}
    end;
handle_info({udp_closed, S}, State = #state{socket = S}) ->
    {stop, udp_closed, State};
handle_info({timeout, Ref, ping}, State = #state{timer = Ref}) ->
    %% we are the chose ones, ping if necessary !!
    {noreply, timer_periodic(maybe_ping(State))};
handle_info({timeout, _, realloc_port}, State) ->
    {noreply, open_udp_port(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, #state{socket = S}) -> gen_udp:close(S).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
open_udp_port(State = #state{socket = undefined, port = PrimaryPort}) ->
    Ports = bootstrap_lib:get_env(secondary_ports),
    {{ok, Socket}, Port} = open_oneof_udp_ports([PrimaryPort | Ports]),
    ?DBG("Using port ~w.~n", [Port]),
    realloc_port_timer(Port, State#state{socket = Socket});
open_udp_port(State = #state{socket = CurrentSocket, port = PrimaryPort}) ->
    case open_oneof_udp_ports([PrimaryPort]) of
        {{ok, NewSocket}, PrimaryPort} ->
            ?DBG("Using port ~w.~n", [PrimaryPort]),
            gen_udp:close(CurrentSocket),
            State#state{socket = NewSocket};
        {{error, _}, PrimaryPort} ->
            realloc_port_timer(-1, State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
open_oneof_udp_ports(Ports) ->
    ProtocolModule = to_mod(bootstrap_lib:get_env(protocol)),
    PortList = [{ProtocolModule, Port} || Port <- Ports],
    lists:foldl(fun try_open/2, {error, no_ports}, PortList).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
try_open(_, {{ok, Socket}, Port}) ->
    {{ok, Socket}, Port};
try_open({ProtocolModule, Port}, _) ->
    {gen_udp:open(Port, [binary | ProtocolModule:options()]), Port}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_mod(broadcast) -> bootstrap_broadcast;
to_mod(multicast) -> bootstrap_multicast.

%%------------------------------------------------------------------------------
%% @private
%% A timer scheduling a ping with a certain, static time.
%%------------------------------------------------------------------------------
timer_backoff(State = #state{timeout = Timeout}) ->
    ping_timer(max(1500, Timeout + (Timeout div 2)), State).

%%------------------------------------------------------------------------------
%% @private
%% A timer scheduling a ping with a certain, variable time.
%%------------------------------------------------------------------------------
timer_periodic(State = #state{timeout = Timeout}) ->
    ping_timer(max(0, Timeout - 1000) + crypto:rand_uniform(0, 1000), State).

%%------------------------------------------------------------------------------
%% @private
%% A timer scheduling a port reallocation attempt. This is done to always keep
%% the primary port maintained by some node. This is needed for node discovery
%% functioning properly. The primary port can become unoccupied when the node
%% listening on it exits.
%%------------------------------------------------------------------------------
realloc_port_timer(PrimaryPort, State = #state{port = PrimaryPort}) ->
    State;
realloc_port_timer(_SecondaryPort, State) ->
    start_timer(1000, realloc_port),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ping_timer(Millis, State = #state{timer = OldRef}) ->
    case OldRef of undefined -> ok; _ -> erlang:cancel_timer(OldRef) end,
    State#state{timer = start_timer(Millis, ping)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_timer(Millis, Message) -> erlang:start_timer(Millis, self(), Message).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ping(PingNode, PingAddr = {I1, I2, I3, I4}, InPort, State) ->
    [begin
         Msg = term_to_binary(?BOOTSTRAP_PONG(node(), PingNode)),
         ok = gen_udp:send(State#state.socket, PingAddr, Port, Msg),
         ?DBG("Sent PONG to ~w.~w.~w.~w:~w.~n", [I1, I2, I3, I4, Port])
     end || Port <- lists:usort([InPort, State#state.port])],
    maybe_backoff(InPort, PingNode, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_pong(Node, PingNode, State) when PingNode == node() ->
    maybe_connect(Node, State);
handle_pong(Node, PingNode, State) ->
    maybe_backoff(PingNode, maybe_connect(PingNode, maybe_connect(Node, State))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_ping(State = #state{pattern = P, mode = Mode, minimum = M}) ->
    case M == infinity orelse length(bootstrap_lib:matching(P, Mode)) < M of
        true  -> do_ping(State);
        false -> State
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_ping(State = #state{protocol = ProtocolModule}) ->
    do_ping(ProtocolModule:addresses(), State).
do_ping([], State) ->
    ?ERR("No network addresses to send to.", []),
    State;
do_ping(Addresses, State = #state{socket = Socket, port = Port}) ->
    [begin
         Msg = term_to_binary(?BOOTSTRAP_PING(node(), Addr)),
         ok = gen_udp:send(Socket, Addr, Port, Msg),
         ?DBG("Sent PING to ~w.~w.~w.~w:~w.~n", [I1, I2, I3, I4, Port])
     end || Addr = {I1, I2, I3, I4} <- Addresses],
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_connect(Node, State = #state{mode = Mode, pattern = Pattern}) ->
    case {Mode, bootstrap_lib:matches(Node, Pattern)} of
        {visible, true} -> Result = net_kernel:connect_node(Node);
        {hidden, true}  -> Result = net_kernel:hidden_connect_node(Node);
        {_, false}      -> Result = skipped
    end,
    case Result of
        false   -> ?ERR("Failed to connect to matching node ~s.", [Node]);
        true    -> ?DBG("Connected to matching node ~s.~n", [Node]);
        skipped -> ok
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%% This function is called when this node receives a ping from another node. If
%% this ping was sent from a node that listens on the primary port, the function
%% delegates to {@link maybe_backoff/2}, in the other case this node backs off
%% since the other node listens on a secondary port and thus can't receive pings
%% at all, implication is that it must be the pinger.
%%------------------------------------------------------------------------------
maybe_backoff(P, Node, State = #state{port = P}) -> maybe_backoff(Node, State);
maybe_backoff(_Port, _Node, State)               -> timer_backoff(State).

%%------------------------------------------------------------------------------
%% @private
%% This function is called when ping clashes are detected (duplicate pinging).
%% It determines which node is allowed to continue pinging (using a node name
%% comparison). Ping clashes are either detected by {@link handle_ping/4} or
%% {@link handle_pong/3}. If pongs containing a different ping node are received
%% this means that there's another node pinging with the same source port. Only
%% one pinger is allowed.
%%------------------------------------------------------------------------------
maybe_backoff(Node, State) when Node < node() -> timer_backoff(State);
maybe_backoff(_Node, State)                   -> State.
