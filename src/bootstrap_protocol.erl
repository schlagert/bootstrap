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
%%% An unregistered server implementing the UDP based discovery protocol.
%%% This protocol basically consists of two PDUs that are represented by the
%%% following erlang terms encoded in the external binary format:
%%% * `{bootstrap, {ping, Node, From}}':
%%%   This message is issued to find other nodes. The sending nodes node name is
%%%   included as well as the address this request was issued over. This allows
%%%   easy answering as well as separating own packets.
%%% * `{bootstrap, {pong, Node}}':
%%%   This message is the answer to a ping packet and contains the node name of
%%%   the answering node. It is issued to the port the request was sent from.
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

-ifdef(DEBUG).
-define(DBG(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DBG(Fmt, Args), Fmt = Fmt, Args = Args, ok).
-endif.

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
          port     :: inet:port_number(),
          socket   :: inet:socket(),
          skip     :: boolean(),
          minimum  :: non_neg_integer(),
          timeout  :: non_neg_integer()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, timer_initial(
           #state{
              mode     = bootstrap:get_env(connect_mode, ?CONNECT_MODE),
              pattern  = bootstrap:pattern(),
              protocol = to_mod(bootstrap:get_env(protocol, ?PROTOCOL)),
              port     = bootstrap:get_env(primary_port, ?PRIMARY_PORT),
              socket   = element(2, {ok, _} = open_socket()),
              minimum  = bootstrap:get_env(min_connections, ?CONNECTIONS),
              timeout  = bootstrap:get_env(ping_timeout, ?PING_TIMEOUT)})}.

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
        {_, ?BOOTSTRAP_PING(Node, _)} when Node == node() ->
            {noreply, State};
        {_, ?BOOTSTRAP_PING(Node, From)} ->
            ?DBG("Got PING from ~s with source port ~w.~n", [Node, Port]),
            {noreply, handle_ping(From, Port, State)};
        {_, ?BOOTSTRAP_PONG(Node)} when Node == node() ->
            {noreply, State};
        {_, ?BOOTSTRAP_PONG(Node)} ->
            ?DBG("Got PONG from ~s with source port ~w.~n", [Node, Port]),
            {noreply, handle_pong(Node, State)};
        {{I1, I2, I3, I4}, Msg} ->
            ?DBG("Ignoring ~w from ~w.~w.~w.~w:~w.~n",
                 [Msg, I1, I2, I3, I4, Port]),
            {noreply, State}
    catch
        _:_ -> {noreply, State}
    end;
handle_info({udp_closed, S}, State = #state{socket = S}) ->
    {stop, udp_closed, State};
handle_info(ping_timeout, State) ->
    {noreply, timer_periodic(maybe_ping(State))};
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
open_socket() ->
    Port = bootstrap:get_env(primary_port, ?PRIMARY_PORT),
    Ports = bootstrap:get_env(secondary_ports, ?SECONDARY_PORTS),
    ProtocolModule = to_mod(bootstrap:get_env(protocol, ?PROTOCOL)),
    PortList = [{ProtocolModule, P} || P <- [Port | Ports]],
    lists:foldl(fun try_open/2, {error, no_ports}, PortList).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
try_open(_, {ok, Socket}) ->
    {ok, Socket};
try_open({ProtocolModule, Port}, _) ->
    gen_udp:open(Port, [binary | ProtocolModule:options()]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_mod(broadcast) -> bootstrap_broadcast;
to_mod(multicast) -> bootstrap_multicast.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
timer_initial(State = #state{timeout = Timeout}) ->
    timer(max(1000, Timeout) + (Timeout div 2), State).
timer_periodic(State = #state{timeout = Timeout}) ->
    timer(max(1000, Timeout - 1000) + crypto:rand_uniform(0, 1000), State).
timer(Timeout, State) ->
    erlang:send_after(Timeout, self(), ping_timeout),
    State#state{skip = false}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ping(Addr = {I1, I2, I3, I4}, InPort, State = #state{socket = Socket}) ->
    [begin
         Msg = term_to_binary(?BOOTSTRAP_PONG(node())),
         ok = gen_udp:send(Socket, Addr, Port, Msg),
         ?DBG("Sent PONG to ~w.~w.~w.~w:~w.~n", [I1, I2, I3, I4, Port])
     end || Port <- lists:usort([InPort, State#state.port])],
    State#state{skip = true}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_pong(Node, State = #state{mode = Mode, pattern = Pattern}) ->
    case {Mode, bootstrap:matches(Node, Pattern)} of
        {visible, true} -> Result = net_kernel:connect(Node);
        {hidden, true}  -> Result = net_kernel:hidden_connect(Node);
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
%%------------------------------------------------------------------------------
maybe_ping(State = #state{pattern = P, skip = S, minimum = M}) ->
    case S orelse (M /= infinity andalso length(bootstrap:matching(P)) >= M) of
        false -> do_ping(State);
        true  -> State
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
