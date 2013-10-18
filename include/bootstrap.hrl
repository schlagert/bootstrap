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

-ifndef(bootstrap_hrl_).
-define(bootstrap_hrl_, 1).

%%%=============================================================================
%%% Deines related to application internal logging.
%%%=============================================================================

-define(ERR(Fmt, Args), error_logger:error_msg(Fmt, Args)).

-ifdef(DEBUG).
-define(DBG(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DBG(Fmt, Args), Fmt = Fmt, Args = Args, ok).
-endif.

%%%=============================================================================
%%% Define the application internal bootstrap event handler {Module, Id}.
%%%=============================================================================

-define(BOOTSTRAP_HANDLER(Module), {bootstrap_event, Module}).

%%%=============================================================================
%%% Define the application internal bootstrap event handler state.
%%%=============================================================================

-record(bootstrap_handler, {pid :: pid(), module :: module(), arg :: term()}).

%%%=============================================================================
%%% Define the application internal bootstrap protocol.
%%%=============================================================================

-define(BOOTSTRAP_PING(PingNode, PingAddr), {bootstrap, {ping, PingNode, PingAddr}}).
-define(BOOTSTRAP_PONG(Node, PingNode),     {bootstrap, {pong, Node, PingNode}}).

%%%=============================================================================
%%% Defines configuration defaults.
%%%=============================================================================

-define(CONNECT_REGEX,   ".*").
-define(CONNECT_MODE,    visible).
-define(CONNECTIONS,     infinity).
-define(PROTOCOL,        broadcast).
-define(PRIMARY_PORT,    50337).
-define(SECONDARY_PORTS, [50338, 50339]).
-define(PING_TIMEOUT,    10000).
-define(MULTICAST_IP,    {239, 192, 0, 1}).
-define(MULTICAST_TTL,   1).

-endif. %% bootstrap_hrl_
