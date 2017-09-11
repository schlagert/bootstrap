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
%%%=============================================================================

-ifndef(bootstrap_hrl_).
-define(bootstrap_hrl_, 1).

%%%=============================================================================
%%% Defines related to application internal logging.
%%%=============================================================================

-define(ERR(Fmt, Args), ok = error_logger:error_msg(Fmt, Args)).

-ifdef(DEBUG).
-define(DBG(Fmt, Args), ok = io:format(standard_error, Fmt, Args)).
-else.
-define(DBG(Fmt, Args), Fmt = Fmt, Args = Args, ok).
-endif.

-ifdef(TEST).
-define(INFO(Fmt, Args), ok = io:format(standard_error, Fmt, Args)).
-else.
-define(INFO(Fmt, Args), ok = io:format(Fmt, Args)).
-endif.

%%%=============================================================================
%%% Define messages sent from bootstrap to bootstrap handlers.
%%%=============================================================================

-define(BOOTSTRAP_UP(Node),           {bootstrap, {nodeup, Node}}).
-define(BOOTSTRAP_DOWN(Node, Reason), {bootstrap, {nodedown, Node, Reason}}).

%%%=============================================================================
%%% Define the application internal bootstrap protocol (distributed over
%%% network).
%%%=============================================================================

-define(BOOTSTRAP_PING(PingNode, PingAddr), {bootstrap, {ping, PingNode, PingAddr}}).
-define(BOOTSTRAP_PONG(Node, PingNode),     {bootstrap, {pong, Node, PingNode}}).

-endif. %% bootstrap_hrl_
