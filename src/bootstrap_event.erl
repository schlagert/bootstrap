%%%=============================================================================
%%% Copyright (c) 2013 Tobias Schlager <schlagert@github.com>
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
%%% A `gen_event' manager and wrapper to distribute node connection messages.
%%% @end
%%%=============================================================================
-module(bootstrap_event).

-behaviour(gen_event).

%% Registration API
-export([start_link/0,
         add/1,
         delete/1,
         list/0]).

%% Publishing API
-export([on_connected/1,
         on_connected/2,
         on_disconnected/2]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("bootstrap.hrl").

%%%=============================================================================
%%% Registration API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts a locally registered generic event manager.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_event:start_link({local, ?MODULE}).

%%------------------------------------------------------------------------------
%% @doc
%% Add a bootstrap event handler to this event manager. If the passed handler
%% structure contains a process id the handler registration is supervised. If
%% the given module is already registered, adding the handler will fail.
%% @end
%%------------------------------------------------------------------------------
-spec add(#bootstrap_handler{}) -> ok | term().
add(Handler = #bootstrap_handler{module = Module, pid = undefined}) ->
    add(add_handler, Module, Handler);
add(Handler = #bootstrap_handler{module = Module}) ->
    add(add_sup_handler, Module, Handler).
add(Function, Module, Handler) ->
    case lists:member(Module, list()) of
        false ->
            gen_event:Function(?MODULE, ?BOOTSTRAP_HANDLER(Module), Handler);
        true ->
            {already_registered, Module}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Removes an event handler from this event manager, specified by the callback
%% module implementing the {@link bootstrap} behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec delete(module()) -> ok | term().
delete(Module) ->
    gen_event:delete_handler(?MODULE, ?BOOTSTRAP_HANDLER(Module), shutdown).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list containing all currently registered bootstrap event handlers.
%% @end
%%------------------------------------------------------------------------------
-spec list() -> [module()].
list() ->
    [Module || ?BOOTSTRAP_HANDLER(Module) <- gen_event:which_handlers(?MODULE)].

%%%=============================================================================
%%% Publishing API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Publishes that a node matching the `connect_to' regex connected.
%% @end
%%------------------------------------------------------------------------------
-spec on_connected(node()) -> ok.
on_connected(Node) -> gen_event:notify(?MODULE, {connected, Node}).

%%------------------------------------------------------------------------------
%% @doc
%% Notifies a list of matching, connected nodes to a certain event handler.
%% @end
%%------------------------------------------------------------------------------
-spec on_connected(#bootstrap_handler{}, [node()]) -> ok.
on_connected(_Handler, []) ->
    ok;
on_connected(#bootstrap_handler{module = Module}, Nodes) ->
    spawn(fun() ->
                  H = ?BOOTSTRAP_HANDLER(Module),
                  gen_event:call(?MODULE, H, {connected, Nodes}, infinity)
          end),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Publishes that a node matching the `connect_to' regex disconnected.
%% @end
%%------------------------------------------------------------------------------
-spec on_disconnected(node(), term()) -> ok.
on_disconnected(Node, Reason) ->
    gen_event:notify(?MODULE, {disconnected, Node, Reason}).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Handler) -> {ok, Handler}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({connected, Node}, Handler) ->
    {ok, publish(on_connected, [Node], Handler)};
handle_event({disconnected, Node, Reason}, Handler) ->
    {ok, publish(on_disconnected, [Node, Reason], Handler)};
handle_event(_Event, Handler) ->
    {ok, Handler}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({connected, Nodes}, Handler) ->
    Fun = fun(N, S) -> publish(on_connected, [N], S) end,
    {ok, ok, lists:foldl(Fun, Handler, Nodes)};
handle_call(_Request, Handler) ->
    {ok, undef, Handler}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, Handler) -> {ok, Handler}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(stop, Handler) ->
    Current = bootstrap:get_env(handlers, []),
    bootstrap:set_env(handlers, [Handler | Current]);
terminate(_Arg, _Handler) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, Handler, _Extra) -> {ok, Handler}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
publish(Fun, Args, Handler = #bootstrap_handler{module = Module}) ->
    try erlang:apply(Module, Fun, Args ++ [Handler#bootstrap_handler.arg]) of
        NewArg -> Handler#bootstrap_handler{arg = NewArg}
    catch
        _:_ -> Handler
    end.
