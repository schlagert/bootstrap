#!/usr/bin/env escript
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
%%% An example escript starting a node that discovers other instances using the
%%% `bootstrap' application.
%%% @end
%%%=============================================================================

-mode(compile).

%% API
-export([main/1]).

%% bootstrap callbacks
-export([on_connected/2, on_disconnected/3]).

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
main([NodeName]) ->
    main([NodeName, ".*"]);
main([NodeName, Regex]) ->
    main([NodeName, Regex, "visible"]);
main([NodeName, Regex, Mode]) ->
    main([NodeName, Regex, Mode, "broadcast"]);
main([NodeName, Regex, Mode, Protocol]) ->
    ?LOG("Starting net_kernel with name ~s~n", [NodeName]),
    {ok, _} = net_kernel:start([list_to_atom(NodeName), longnames]),
    true = code:add_path(get_ebin_dir(element(2, {ok, _} = file:get_cwd()))),
    application:load(sasl),
    ok = application:set_env(sasl, sasl_error_logger, false),
    application:start(sasl),
    application:start(crypto),
    ok = application:load(bootstrap),
    ok = bootstrap:set_env(connect_regex, Regex),
    ok = bootstrap:set_env(connect_mode, list_to_existing_atom(Mode)),
    ok = bootstrap:set_env(protocol, list_to_existing_atom(Protocol)),
    ok = application:start(bootstrap),
    ok = bootstrap:add_sup_handler(?MODULE, self()),
    main_loop();
main(_) ->
    io:format(
      "~nA script to demonstrate the bootstrap application.~n~n"
      "Usage:  ~s NodeName [ConnectRegex] [hidden|visible] [broadcast|multicast]~n"
      "     with NodeName     - A name suitable for long names mode.~n"
      "          ConnectRegex - A regex string for nodes to connect to.~n~n",
      [escript:script_name()]).

%%%=============================================================================
%%% bootstrap callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
on_connected(Node, Pid) ->
    Pid ! {connected, Node},
    Pid.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
on_disconnected(Node, Reason, Pid) ->
    Pid ! {disconnected, Node, Reason},
    Pid.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
main_loop() ->
    receive
        {connected, Node} ->
            ?LOG("Node ~s connected.~n", [Node]);
        {disconnected, Node, Reason} ->
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
