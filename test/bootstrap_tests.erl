%%%=============================================================================
%%% Copyright 2017, Tobias Schlager <schlagert@github.com>
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
%%%=============================================================================

-module(bootstrap_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bootstrap/include/bootstrap.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

broadcast_test_() ->
    {timeout, 30, {spawn, fun() -> connect(broadcast) end}}.

multicast_test_() ->
    {timeout, 30, {spawn, fun() -> connect(multicast) end}}.

connect(Protocol) ->
    process_flag(trap_exit, true),

    %% setup master node, master will not participate in bootstrap process
    {ok, Master} = distribute('test0@localhost'),

    %% setup slave nodes with bootstrap running
    {ok, Slave1} = slave_setup(test1, Protocol),
    {ok, Slave2} = slave_setup(test2, Protocol),

    %% Initially, the slave module sets up a fully connected mesh, we wait until
    %% this gets reported by the bootstrap handlers on both slaves.
    spawn(Slave1, handler(self())),
    spawn(Slave2, handler(self())),
    receive {Slave1, ?BOOTSTRAP_UP(Master)} -> ok after 5000 -> throw(fail) end,
    receive {Slave1, ?BOOTSTRAP_UP(Slave1)} -> ok after 5000 -> throw(fail) end,
    receive {Slave1, ?BOOTSTRAP_UP(Slave2)} -> ok after 5000 -> throw(fail) end,
    receive {Slave2, ?BOOTSTRAP_UP(Master)} -> ok after 5000 -> throw(fail) end,
    receive {Slave2, ?BOOTSTRAP_UP(Slave1)} -> ok after 5000 -> throw(fail) end,
    receive {Slave2, ?BOOTSTRAP_UP(Slave2)} -> ok after 5000 -> throw(fail) end,

    %% Now we break the connections between the slaves manually (connections to
    %% the master can not be broken, unless you want the slave to exit)
    Separate = fun() ->
                       true = net_kernel:connect_node(Slave2),
                       true = net_kernel:disconnect(Slave2)
               end,
    ok = slave_execute(Slave1, Separate),
    receive {Slave1, ?BOOTSTRAP_DOWN(Slave2, disconnect)} -> ok
    after 5000 -> throw(fail) end,
    receive {Slave2, ?BOOTSTRAP_DOWN(Slave1, connection_closed)} -> ok
    after 5000 -> throw(fail) end,

    Nodes = fun() -> exit({shutdown, [node() | nodes()]}) end,
    {ok, Nodes1AfterDisconnect} = slave_execute(Slave1, Nodes),
    {ok, Nodes2AfterDisconnect} = slave_execute(Slave2, Nodes),
    ?assertEqual([Master, Slave1], lists:usort(Nodes1AfterDisconnect)),
    ?assertEqual([Master, Slave2], lists:usort(Nodes2AfterDisconnect)),

    %% Now we do nothing and wait for recovery powered by bootstrap
    receive {Slave1, ?BOOTSTRAP_UP(Slave2)} -> ok after 5000 -> throw(fail) end,
    receive {Slave2, ?BOOTSTRAP_UP(Slave1)} -> ok after 5000 -> throw(fail) end,

    {ok, Nodes1AfterReconnect} = slave_execute(Slave1, Nodes),
    {ok, Nodes2AfterReconnect} = slave_execute(Slave2, Nodes),
    ?assertEqual(lists:usort(Nodes1AfterReconnect),
                 lists:usort(Nodes2AfterReconnect)),
    ?assertEqual([Master, Slave1, Slave2], lists:usort(Nodes1AfterReconnect)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handler(ParentHandler) ->
    fun() ->
            ok = bootstrap:monitor_nodes(true),
            handler_loop(ParentHandler)
    end.
handler_loop(ParentHandler) ->
    receive
        Msg ->
            ParentHandler ! {node(), Msg},
            handler_loop(ParentHandler)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_apps(Protocol) ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, false),
    application:start(sasl),
    application:start(crypto),
    application:load(bootstrap),
    application:set_env(bootstrap, primary_port, 40001),
    application:set_env(bootstrap, secondary_ports, [40002]),
    application:set_env(bootstrap, connect_regex, "test"),
    application:set_env(bootstrap, protocol, Protocol),
    application:set_env(bootstrap, min_connections, 3),
    application:set_env(bootstrap, ping_timeout, 1000),
    application:start(bootstrap).

%%------------------------------------------------------------------------------
%% @private
%% Make this node a distributed node.
%%------------------------------------------------------------------------------
distribute(Name) ->
    os:cmd("epmd -daemon"),
    case net_kernel:start([Name, shortnames]) of
        {ok, _}                       -> {ok, node()};
        {error, {already_started, _}} -> {ok, node()};
        Error                         -> Error
    end.

%%------------------------------------------------------------------------------
%% @private
%% Start a slave node and setup its environment (code path, applications, ...).
%%------------------------------------------------------------------------------
slave_setup(Name, Protocol) ->
    Arg = string:join(["-pa " ++ P || P <- code:get_path()], " "),
    {ok, Node} = slave:start_link(localhost, Name, Arg),
    %% Make sure slave node started correctly and is now connected
    true = lists:member(Node, nodes()),
    %% Start the needed applications
    ok = slave_execute(Node, fun() -> setup_apps(Protocol) end),
    {ok, Node}.

%%------------------------------------------------------------------------------
%% @private
%% Execute `Fun' on the given node.
%%------------------------------------------------------------------------------
slave_execute(Node, Fun) ->
    Pid = spawn_link(Node, Fun),
    receive
        {'EXIT', Pid, normal}             -> ok;
        {'EXIT', Pid, {shutdown, Result}} -> {ok, Result};
        {'EXIT', Pid, Reason}             -> {error, Reason}
    end.
