#!/usr/bin/env escript
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
%%% An example escript starting a node that discovers other instances using the
%%% `bootstrap' application.
%%% @end
%%%=============================================================================

-mode(compile).

%% API
-export([main/1]).

-define(LOG(Fmt, Args), io:format(Fmt, Args)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Main function that extends the code path, makes the node distributed and
%% starts the needed application. All node actions will be logging to stdout.
%% @end
%%------------------------------------------------------------------------------
main([NodeName, Cookie]) ->
    main([NodeName, Cookie, ".*"]);
main([NodeName, Cookie, Regex]) ->
    main([NodeName, Cookie, Regex, "visible"]);
main([NodeName, Cookie, Regex, Mode]) ->
    main([NodeName, Cookie, Regex, Mode, "broadcast"]);
main([NodeName, Cookie, Regex, Mode, Protocol]) ->
    ?LOG("Starting net_kernel with name ~s~n", [NodeName]),
    {ok, _} = net_kernel:start([list_to_atom(NodeName), longnames]),
    true = erlang:set_cookie(node(), list_to_atom(Cookie)),
    true = code:add_path(get_ebin_dir(element(2, {ok, _} = file:get_cwd()))),
    application:load(sasl),
    ok = application:set_env(sasl, sasl_error_logger, false),
    application:start(sasl),
    application:start(crypto),
    ok = application:load(bootstrap),
    ok = application:set_env(bootstrap, connect_regex, Regex),
    ModeAtom = list_to_existing_atom(Mode),
    ok = application:set_env(bootstrap, connect_mode, ModeAtom),
    ProtocolAtom = list_to_existing_atom(Protocol),
    ok = application:set_env(bootstrap, protocol, ProtocolAtom),
    ok = application:start(bootstrap),
    ok = bootstrap:monitor_nodes(true),
    main_loop();
main(_) ->
    io:format(
      "~nA script to demonstrate the bootstrap application.~n~n"
      "Usage:  ~s NodeName Cookie [ConnectRegex] [ConnnectMode] [Protocol]~n"
      "     with NodeName     - a name suitable for long names mode~n"
      "          Cookie       - the nodes magic cookie~n"
      "          ConnectRegex - regex string for nodes to connect to~n"
      "          ConnectMode  - type of node connections (hidden or visible)~n"
      "          Protocol     - network protocol to use (broadcast or  multicast)~n"
      "~n",
      [escript:script_name()]).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
main_loop() ->
    receive
        {bootstrap, {nodeup, Node}} ->
            ?LOG("Node ~s connected.~n", [Node]);
        {bootstrap, {nodedown, Node, Reason}} ->
            ?LOG("Node ~s disconnected with reason ~p.~n", [Node, Reason])
    end,
    io:format("~n~.80c~n", [$=]),
    bootstrap:info(),
    io:format("~.80c~n~n", [$=]),
    main_loop().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_ebin_dir(Cwd) ->
    get_ebin_dir_(lists:reverse(filename:split(Cwd))).
get_ebin_dir_(Path = ["bootstrap" | _]) ->
    filename:join(lists:reverse(["ebin" | Path]));
get_ebin_dir_(["example" | Path]) ->
    get_ebin_dir_(Path).
