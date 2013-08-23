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
%%% A module providing handler storage and (local) persistence facilities.
%%% @end
%%%=============================================================================
-module(bootstrap_handlers).

%% API
-export([init/1, add/2, delete/1, update/2, to_list/0]).

-define(TABLE_ID, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Initialize the handler administration data structures. This function will
%% fail when it gets called more than once. Cleanup is not necessary, since the
%% data structure will be removed when the application exits.
%% @end
%%------------------------------------------------------------------------------
-spec init([{module(), State :: term()}]) -> ok.
init(Handlers) ->
    ets:new(?TABLE_ID, [named_table, public]),
    ets:insert(?TABLE_ID, Handlers),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Adds a new bootstrap handler module. A specific module can only
%% be added once per time. If the same module was already added before it must
%% be deleted in advance.
%% @end
%%------------------------------------------------------------------------------
-spec add(module(), State :: term()) -> ok | {error, term()}.
add(Module, State) ->
    case ets:insert_new(?TABLE_ID, {Module, State}) of
        false ->
            {error, {already_present, Module}};
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Deletes a bootstrap handler module.
%% @end
%%------------------------------------------------------------------------------
-spec delete(module()) -> ok.
delete(Module) -> ets:delete(?TABLE_ID, Module), ok.

%%------------------------------------------------------------------------------
%% @doc
%% Updates a bootstrap handler entry. This is used for state updates and should
%% only be called by {@link bootstrap_protocol}. The race condition of removing
%% a handler from somewhere and accidentially re-adding it by update is handled
%% in the way that only existing entries are updated.
%% @end
%%------------------------------------------------------------------------------
-spec update(module(), State :: term()) -> ok.
update(Module, State) -> ets:update_element(?TABLE_ID, Module, {2, State}), ok.

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all registered bootstrap handlers and their current state.
%% @end
%%------------------------------------------------------------------------------
-spec to_list() -> [{module(), State :: term()}].
to_list() -> ets:tab2list(?TABLE_ID).
