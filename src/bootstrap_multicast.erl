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
%%% An implementation of the {@link bootstrap_protocol} providing UDP multicast
%%% functionality.
%%% @end
%%%=============================================================================
-module(bootstrap_multicast).

-behaviour(bootstrap_protocol).

%% bootstrap_protocol callbacks
-export([options/0, addresses/0]).

-include("bootstrap.hrl").

%%%=============================================================================
%%% bootstrap_protocol callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
options() ->
    Addr = bootstrap_lib:get_env(multicast_ip),
    Iface = bootstrap_lib:get_env(multicast_iface, {0, 0, 0, 0}),
    [{add_membership, {Addr, Iface}},
     {multicast_ttl, bootstrap_lib:get_env(multicast_ttl)},
     {multicast_loop, true}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
addresses() -> [bootstrap_lib:get_env(multicast_ip)].
