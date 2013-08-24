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
%%% A server managing bootstrap handler registrations.
%%% @end
%%%=============================================================================
-module(bootstrap_reg).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 add/2,
	 add_sup/2,
	 delete/1,
	 get/0,
	 update/1]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start a registered server that keeps track of bootstrap handler
%% registrations. Registered handlers will be saved in the application
%% environment to be able to savely restart the server (and the application).
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% Add a handler module. Duplicate module registrations will not be accepted.
%% @end
%%------------------------------------------------------------------------------
-spec add(module(), State :: term()) -> ok | {error, term()}.
add(Module, State) ->
    gen_server:call(?MODULE, {add, {{Module, State}, undefined}}).

%%------------------------------------------------------------------------------
%% @doc
%% Add a handler module. Handlers added with this function will automatically
%% be removed when the calling process exits. Duplicate module registrations
%% will not be accepted.
%% @end
%%------------------------------------------------------------------------------
-spec add_sup(module(), State :: term()) -> ok | {error, term()}.
add_sup(Module, State) ->
    gen_server:call(?MODULE, {add, {{Module, State}, {self(), undefined}}}).

%%------------------------------------------------------------------------------
%% @doc
%% Deletes a registered handler module. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec delete(module()) -> ok.
delete(Module) -> gen_server:cast(?MODULE, {delete, Module}).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all bootstrap handlers and their current state.
%% @end
%%------------------------------------------------------------------------------
-spec get() -> [{module(), State :: term()}].
get() -> [Handler || {Handler, _} <- gen_server:call(?MODULE, get)].

%%------------------------------------------------------------------------------
%% @doc
%% Update the states of the given bootstrap handlers. This will only update
%% existing handler entries and will not add new handlers. This function never
%% fails.
%% @end
%%------------------------------------------------------------------------------
-spec update([{module(), State :: term()}]) -> ok.
update(Handlers) -> gen_server:cast(?MODULE, {update, Handlers}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-type entry()     :: {{module(), term()}, undefined}.
-type sup_entry() :: {{module(), term()}, {pid(), reference()}}.

-record(state, {handlers = [] :: [entry() | sup_entry()]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    SavedHs = bootstrap:get_env(handlers, []),
    AddFun = fun(Entry, State) -> element(2, add_impl(Entry, State)) end,
    {ok, lists:foldr(AddFun, #state{}, SavedHs)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({add, Handler}, _From, State) ->
    {Reply, NewState} = add_impl(Handler, State),
    {reply, Reply, NewState};
handle_call(get, _From, State = #state{handlers = Hs}) ->
    {reply, lists:reverse(Hs), State};
handle_call(_Request, _From, State) ->
    {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({delete, Module}, State) ->
    {noreply, delete_impl(Module, State)};
handle_cast({update, Handlers}, State) ->
    {noreply, update_impl(Handlers, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({'DOWN', _, process, Pid, _}, State) ->
    {noreply, delete_impl(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, #state{handlers = Hs}) -> bootstrap:set_env(handlers, Hs).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_impl(Entry = {{Module, _}, _}, State) ->
    add_impl(already_added(Module, State), Entry, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_impl(false, Entry = {_, undefined}, State = #state{handlers = Hs}) ->
    {ok, State#state{handlers = [Entry | Hs]}};
add_impl(false, {Handler, {Pid, _}}, State = #state{handlers = Hs}) ->
    Entry = {Handler, {Pid, monitor(process, Pid)}},
    {ok, State#state{handlers = [Entry | Hs]}};
add_impl(true, _, State) ->
    {{error, already_added}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
already_added(Module, #state{handlers = Hs}) ->
    lists:any(entry_predicate(Module), Hs).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
delete_impl(Subject, State = #state{handlers = Hs}) ->
    {DeletedHs, NewHs} = lists:partition(entry_predicate(Subject), Hs),
    [demonitor(Monitor) || {_, {_, Monitor}} <- DeletedHs],
    State#state{handlers = NewHs}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_impl(Updates, State = #state{handlers = Hs}) ->
    Fun = fun({{M, S}, R}) -> {{M, proplists:get_value(M, Updates, S)}, R} end,
    State#state{handlers = lists:map(Fun, Hs)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
entry_predicate(Module) when is_atom(Module) ->
    fun({{M, _}, _}) -> M =:= Module end;
entry_predicate(Pid) when is_pid(Pid) ->
    fun({_, {P, _}}) -> P =:= Pid end.
