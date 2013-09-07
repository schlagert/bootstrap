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

-define(LOG(Fmt, Args), error_logger:info_msg(Fmt, Args)).

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
          mode    :: visible | hidden,
          pattern :: re:mp(),
          port    :: inet:port_number(),
          socket  :: inet:socket(),
          skip    :: boolean(),
          minimum :: non_neg_integer(),
          timeout :: non_neg_integer()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    State = #state{
      mode    = bootstrap:get_env(connect_mode, visible),
      pattern = bootstrap:pattern(),
      port    = bootstrap:get_env(send_port, 50337),
      socket  = element(2, {ok, _} = open_socket()),
      minimum = bootstrap:get_env(min_connections, 2),
      timeout = bootstrap:get_env(ping_timeout, 10000)},
    {ok, timer(0, State)}.

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
handle_info({udp, S, _, Port, Data}, State = #state{socket = S}) ->
    try binary_to_term(Data) of
        ?BOOTSTRAP_PING(Node, From) when Node =/= node() ->
            {noreply, handle_ping(From, Port, State)};
        ?BOOTSTRAP_PONG(Node) when Node =/= node() ->
            {noreply, handle_pong(Node, State)};
        _ ->
            {noreply, State}
    catch
        _:_ -> {noreply, State}
    end;
handle_info({udp_closed, S}, State = #state{socket = S}) ->
    {stop, udp_closed, State};
handle_info(ping_timeout, State) ->
    {noreply, timer(maybe_ping(State))};
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
    Port = bootstrap:get_env(send_port, 50337),
    Ports = bootstrap:get_env(secondary_ports, [50338, 50339]),
    lists:foldl(fun try_open/2, {error, no_ports}, [Port | Ports]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
try_open(_, {ok, Socket}) -> {ok, Socket};
try_open(Port, _)         -> gen_udp:open(Port, [binary, {broadcast, true}]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
timer(State = #state{timeout = Timeout}) ->
    timer(Timeout, State).
timer(Timeout, State) ->
    Rand = crypto:rand_uniform(1000, 2000),
    erlang:send_after(Timeout + Rand, self(), ping_timeout),
    State#state{skip = false}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ping(Addr, Port, State = #state{port = P}) ->
    NewState = send(Addr, Port, term_to_binary(?BOOTSTRAP_PONG(node())), State),
    case Port of P -> NewState#state{skip = true}; _ -> NewState end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_pong(Node, State = #state{mode = Mode, pattern = Pattern}) ->
    case {Mode, bootstrap:matches(Node, Pattern)} of
        {visible, true} -> Result = net_kernel:connect(Node);
        {hidden, true} -> Result = net_kernel:hidden_connect(Node);
        {_, false}     -> Result = skipped
    end,
    case Result of
        false -> ?LOG("Failed to connect to matching node ~s.", [Node]);
        _     -> ok
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_ping(State = #state{skip = Skip, minimum = Min}) ->
    case Skip orelse length(matching(State)) >= Min of
        true  -> State;
        false -> do_ping(State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_ping(State) ->
    do_ping(addresses(), State).
do_ping([], State) ->
    ?LOG("No network addresses to send to.", []),
    State;
do_ping(As, State) ->
    lists:foldl(
      fun(A, S) -> send(A, term_to_binary(?BOOTSTRAP_PING(node(), A)), S) end,
      State, As).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
matching(#state{mode = M, pattern = P}) ->
    bootstrap:matching(P, case M of hidden -> connected; visible -> M end).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send(Addr, Message, State = #state{port = Port}) ->
    send(Addr, Port, Message, State).
send(Addr, Port, Message, State = #state{socket = Socket}) ->
    ok = gen_udp:send(Socket, Addr, Port, Message),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
addresses() ->
    [A || {ok, Is} <- [inet:getifaddrs()],
          {_, L} <- Is,
          {broadaddr, A = {_, _, _, _}} <- L,
          lists:member(up, proplists:get_value(flags, L, []))].
