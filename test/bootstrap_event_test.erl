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
%%%=============================================================================

-module(bootstrap_event_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bootstrap/include/bootstrap.hrl").

-behaviour(bootstrap).

-export([on_connected/2, on_disconnected/3]).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

add_delete_test() ->
    process_flag(trap_exit, true),
    {ok, Pid} = bootstrap_event:start_link(),

    Handler = #bootstrap_handler{module = ?MODULE, arg = self()},
    ok = bootstrap_event:add(Handler),

    {already_registered, ?MODULE} = bootstrap_event:add(Handler),

    [?MODULE] = bootstrap_event:list(),

    ok = bootstrap_event:on_connected(node1),
    receive {connected, node1} -> ok end,

    ok = bootstrap_event:on_connected(Handler, [node2, node3]),
    receive {connected, node2} -> ok end,
    receive {connected, node3} -> ok end,

    ok = bootstrap_event:on_disconnected(node4, reason),
    receive {disconnected, node4, reason} -> ok end,

    ok = bootstrap_event:delete(?MODULE),

    [] = bootstrap_event:list(),

    ok = bootstrap_event:on_connected(node5),
    receive {connected, node5} -> throw(test_failed) after 100 -> ok end,

    exit(Pid, shutdown),
    receive {'EXIT', Pid, shutdown} -> ok end.

add_sup_test() ->
    process_flag(trap_exit, true),
    {ok, Pid} = bootstrap_event:start_link(),

    Handler = #bootstrap_handler{pid = self(), module = ?MODULE, arg = self()},
    RegPid = spawn_link(fun() ->
				bootstrap_event:add(Handler),
				Handler#bootstrap_handler.pid ! registered,
				receive _ -> ok end
			end),

    receive registered -> ok end,
    {already_registered, ?MODULE} = bootstrap_event:add(Handler),

    [?MODULE] = bootstrap_event:list(),

    ok = bootstrap_event:on_connected(node1),
    receive {connected, node1} -> ok end,

    ok = bootstrap_event:on_connected(Handler, [node2, node3]),
    receive {connected, node2} -> ok end,
    receive {connected, node3} -> ok end,

    ok = bootstrap_event:on_disconnected(node4, reason),
    receive {disconnected, node4, reason} -> ok end,

    RegPid ! shutdown,
    receive {'EXIT', RegPid, normal} -> ok end,

    [] = bootstrap_event:list(),

    ok = bootstrap_event:on_connected(node5),
    receive
	Msg = {connected, node5} -> throw({test_failed, Msg})
    after 100 ->
	    ok
    end,

    exit(Pid, shutdown),
    receive {'EXIT', Pid, shutdown} -> ok end.

%%%=============================================================================
%%% test behaviour
%%%=============================================================================

on_connected(Node, Pid) ->
    Pid ! {connected, Node},
    Pid.

on_disconnected(Node, Reason, Pid) ->
    Pid ! {disconnected, Node, Reason},
    Pid.
