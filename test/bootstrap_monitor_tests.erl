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

-module(bootstrap_monitor_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bootstrap/include/bootstrap.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

monitor_demonitor_test() ->
    Handler = self(),

    Pid = start_link(),

    %% register handler
    [] = bootstrap_monitor:handlers(),
    ok = bootstrap_monitor:monitor_nodes(true, Handler),
    [Handler] = bootstrap_monitor:handlers(),

    %% unregister handler
    ok = bootstrap_monitor:monitor_nodes(false, Handler),
    [] = bootstrap_monitor:handlers(),

    ok = stop(Pid).

nodeup_after_monitor_test() ->
    Node = 'joe@erlang.org',
    Handler = self(),

    Pid = start_link(),

    %% register handler
    [] = bootstrap_monitor:handlers(),
    ok = bootstrap_monitor:monitor_nodes(true, Handler),
    [Handler] = bootstrap_monitor:handlers(),

    %% fake nodeup for Node... handler receives notification
    Pid ! {nodeup, Node, []},
    receive ?BOOTSTRAP_UP(Node) -> ok end,

    %% unregister handler
    ok = bootstrap_monitor:monitor_nodes(false, Handler),
    [] = bootstrap_monitor:handlers(),

    %% fake nodedown for Node... doesn't get delivered to anybody
    Pid ! {nodedown, Node, [{nodedown_reason, expected}]},
    receive
        Msg = ?BOOTSTRAP_DOWN(Node, _) -> throw({unexpected, Msg})
    after
        100 -> ok
    end,

    ok = stop(Pid).

nodeup_before_monitor_test() ->
    Node = 'joe@erlang.org',
    Handler = self(),

    Pid = start_link(),

    %% fake nodeup for Node...
    Pid ! {nodeup, Node, []},

    %% register handler
    [] = bootstrap_monitor:handlers(),
    ok = bootstrap_monitor:monitor_nodes(true, Handler),
    [Handler] = bootstrap_monitor:handlers(),

    %% handler receives notification for already connected matching node
    receive ?BOOTSTRAP_UP(Node) -> ok end,

    %% fake nodedown for Node, also gets delivered
    Pid ! {nodedown, Node, [{nodedown_reason, expected}]},
    receive ?BOOTSTRAP_DOWN(Node, expected) -> ok end,

    %% unregister handler
    ok = bootstrap_monitor:monitor_nodes(false, Handler),
    [] = bootstrap_monitor:handlers(),

    ok = stop(Pid).

automatic_unregistration_test() ->
    {Handler, HandlerRef} = spawn_monitor(fun() -> receive die -> ok end end),

    Pid = start_link(),

    %% register handler
    [] = bootstrap_monitor:handlers(),
    ok = bootstrap_monitor:monitor_nodes(true, Handler),
    [Handler] = bootstrap_monitor:handlers(),

    %% handler exits
    Handler ! die,
    receive {'DOWN', HandlerRef, process, Handler, normal} -> ok end,
    [] = bootstrap_monitor:handlers(),

    ok = stop(Pid).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_link() ->
    ok = application:set_env(bootstrap, connect_mode, visible),
    ok = application:set_env(bootstrap, connect_regex, ".*"),
    element(2, {ok, _} = bootstrap_monitor:start_link()).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(Pid) ->
    unlink(Pid),
    Ref = monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', Ref, process, Pid, shutdown} -> ok end.
