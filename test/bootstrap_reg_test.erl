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

-module(bootstrap_reg_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

add_delete_resurrect_test() ->
    process_flag(trap_exit, true),
    ok = application:set_env(bootstrap, handlers, []),

    {ok, Pid1} = bootstrap_reg:start_link(),
    ok = bootstrap_reg:add(module_a, state_a),
    [{module_a, state_a}] = bootstrap_reg:get(),

    ok = bootstrap_reg:add(module_b, state_b),
    [{module_a, state_a}, {module_b, state_b}] = bootstrap_reg:get(),

    {error, already_added} = bootstrap_reg:add(module_a, state_c),
    [{module_a, state_a}, {module_b, state_b}] = bootstrap_reg:get(),

    ok = bootstrap_reg:delete(module_a),
    [{module_b, state_b}] = bootstrap_reg:get(),

    ok = bootstrap_reg:add(module_a, state_a),
    [{module_b, state_b}, {module_a, state_a}] = bootstrap_reg:get(),

    exit(Pid1, shutdown),
    receive {'EXIT', Pid1, shutdown} -> ok end,
    
    {ok, Pid2} = bootstrap_reg:start_link(),
    [{module_b, state_b}, {module_a, state_a}] = bootstrap_reg:get(),
    exit(Pid2, shutdown),
    receive {'EXIT', Pid2, shutdown} -> ok end.

add_sup_delete_resurrect_test() ->
    process_flag(trap_exit, true),
    ok = application:set_env(bootstrap, handlers, []),

    {ok, Pid1} = bootstrap_reg:start_link(),
    Worker = spawn_link(
	       fun() ->
		       ok = bootstrap_reg:add_sup(module_a, state_a),
		       receive stop -> ok end
	       end),
    timer:sleep(100),
    [{module_a, state_a}] = bootstrap_reg:get(),
    Worker ! stop,
    receive {'EXIT', Worker, normal} -> ok end,
    [] = bootstrap_reg:get(),

    ok = bootstrap_reg:add(module_b, state_b),
    [{module_b, state_b}] = bootstrap_reg:get(),

    {error, already_added} = bootstrap_reg:add(module_b, state_c),
    [{module_b, state_b}] = bootstrap_reg:get(),

    exit(Pid1, shutdown),
    receive {'EXIT', Pid1, shutdown} -> ok end,
    
    {ok, Pid2} = bootstrap_reg:start_link(),
    [{module_b, state_b}] = bootstrap_reg:get(),
    exit(Pid2, shutdown),
    receive {'EXIT', Pid2, shutdown} -> ok end.

%%%=============================================================================
%%% internal functions
%%%=============================================================================
