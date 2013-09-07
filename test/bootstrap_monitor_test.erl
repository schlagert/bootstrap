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

-module(bootstrap_monitor_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bootstrap/include/bootstrap.hrl").

-behaviour(bootstrap).

-export([on_connected/2, on_disconnected/3]).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

add_no_connect_to_regex_test() ->
    process_flag(trap_exit, true),
    {ok, EvtPid} = bootstrap_event:start_link(),
    {ok, MonPid} = bootstrap_monitor:start_link(),

    Handler = #bootstrap_handler{module = ?MODULE, arg = self()},
    ok = bootstrap_monitor:add(Handler),
    receive {connected, Node} when Node =:= node() -> ok end,

    {error, {already_registered, ?MODULE}} = bootstrap_monitor:add(Handler),

    exit(MonPid, shutdown),
    receive {'EXIT', MonPid, shutdown} -> ok end,
    exit(EvtPid, shutdown),
    receive {'EXIT', EvtPid, shutdown} -> ok end.

add_with_connect_to_regex_test() ->
    process_flag(trap_exit, true),

    ok = bootstrap:set_env(connect_to, "test.*@.*"),

    {ok, EvtPid} = bootstrap_event:start_link(),
    {ok, MonPid} = bootstrap_monitor:start_link(),

    Handler = #bootstrap_handler{module = ?MODULE, arg = self()},
    ok = bootstrap_monitor:add(Handler),

    MonPid ! {nodeup, 'test1@host.domain', [{node_type, visible}]},
    receive {connected, 'test1@host.domain'} -> ok end,

    MonPid ! {nodeup, 'test2@host.domain', [{node_type, visible}]},
    receive {connected, 'test2@host.domain'} -> ok end,

    MonPid ! {nodedown, 'test2@host.domain', [{nodedown_reason, reason}]},
    receive {disconnected, 'test2@host.domain', reason} -> ok end,

    exit(MonPid, shutdown),
    receive {'EXIT', MonPid, shutdown} -> ok end,
    exit(EvtPid, shutdown),
    receive {'EXIT', EvtPid, shutdown} -> ok end.

add_supervised_test() ->
    process_flag(trap_exit, true),

    ok = bootstrap:set_env(connect_to, "test.*@.*"),

    {ok, EvtPid} = bootstrap_event:start_link(),
    {ok, MonPid} = bootstrap_monitor:start_link(),

    Handler = #bootstrap_handler{module = ?MODULE, arg = self()},
    RegPid = spawn_link(
               fun() ->
                       H = Handler#bootstrap_handler{pid = self()},
                       bootstrap_event:add(H),
                       H#bootstrap_handler.arg ! registered,
                       receive _ -> ok end
               end),
    receive registered -> ok end,

    {error, {already_registered, ?MODULE}} = bootstrap_monitor:add(Handler),

    MonPid ! {nodeup, 'test1@host.domain', [{node_type, visible}]},
    receive {connected, 'test1@host.domain'} -> ok end,

    RegPid ! shutdown,
    receive {'EXIT', RegPid, normal} -> ok end,

    MonPid ! {nodedown, 'test1@host.domain', [{nodedown_reason, reason}]},
    receive
        {disconnected, 'test1@host.domain', reason} -> throw(test_failed)
    after
        100 -> ok
    end,

    exit(MonPid, shutdown),
    receive {'EXIT', MonPid, shutdown} -> ok end,
    exit(EvtPid, shutdown),
    receive {'EXIT', EvtPid, shutdown} -> ok end.

%%%=============================================================================
%%% test behaviour
%%%=============================================================================

on_connected(Node, Pid) ->
    Pid ! {connected, Node},
    Pid.

on_disconnected(Node, Reason, Pid) ->
    Pid ! {disconnected, Node, Reason},
    Pid.
