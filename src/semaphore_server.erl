%% @doc
-module(semaphore_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/semaphore.hrl").

-type call()      :: {checkout, key(), ctor(), dtor()} | info.
-type resources() :: gb_tree(). %% gb_tree(key(), {resource(), dtor(), [pid()]}).
-type index()     :: gb_tree(). %% gb_tree(pid(), [key()]).
-type state()     :: {resources(), index()}.

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, state()}.
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, {gb_trees:empty(), gb_trees:empty()}}.

-spec handle_call(call(), _, state()) -> {reply, resource() | state(), state()}.
%% @hidden
handle_call({checkout, Key, Ctor, Dtor}, {Pid, _Tag}, State) ->
    {Reply, NewState} = checkout(Pid, Key, Ctor, Dtor, State),
    {reply, Reply, NewState};
handle_call(info, _From, State) ->
    {reply, info(State), State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info({'DOWN', reference(), process, _, _}, state()) -> {noreply, state()}.
%% @hidden
handle_info({'DOWN', _Ref, process, From, _Reason}, State) ->
    {noreply, checkin(From, State)}.

-spec terminate(_, state()) -> ok.
%% @hidden
terminate(_Reason, {Resources, _Index}) ->
    dispose_all(Resources).

-spec code_change(_, state(), _) -> {ok, state()}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec info(state()) -> [{pid(), [key()]}].
%% @private
info({_Resources, Index}) ->
    [{P, gb_sets:to_list(K)} || {P, K} <- gb_trees:to_list(Index)].

-spec checkout(pid(), key(), ctor(), dtor(), state()) -> {resource(), state()}.
%% @private Find or instantiate a resource
checkout(Owner, Key, Ctor, Dtor, {Resources, Index}) ->
    monitor(process, Owner),
    NewIndex = index(Owner, Key, Index),
    case gb_trees:lookup(Key, Resources) of
        {value, {Res, Dtor, Pids}} ->
            Value = {Res, Dtor, gb_sets:add(Owner, Pids)},
            {Res, {gb_trees:update(Key, Value, Resources), NewIndex}};
        none ->
            Res = Ctor(),
            Value = {Res, Dtor, gb_sets:from_list([Owner])},
            {Res, {gb_trees:insert(Key, Value, Resources), NewIndex}}
    end.

-spec index(pid(), key(), index()) -> index().
%% @private Associate Key with Owner for reverse lookup
index(Owner, Key, Index) ->
    case gb_trees:lookup(Owner, Index) of
        {value, Keys} -> gb_trees:update(Owner, gb_sets:add(Key, Keys), Index);
        none          -> gb_trees:insert(Owner, gb_sets:from_list([Key]), Index)
    end.

-spec checkin(pid(), state()) -> state().
%% @private
checkin(Owner, {Resources, Index}) ->
    {UsedKeys, NewIndex} = deindex(Owner, Index),
    {dispose(Owner, UsedKeys, Resources), NewIndex}.

-spec deindex(pid(), index()) -> {[key()], index()}.
%% @private
deindex(Owner, Index) ->
    %% Get a list of keys assigned to Owner
    Keys = gb_sets:to_list(gb_trees:get(Owner, Index)),
    %% Remove Owner from Index
    {Keys, gb_trees:delete(Owner, Index)}.

-spec dispose(pid(), [key()], resources()) -> resources().
%% @private
dispose(_Owner, [], Resources) ->
    Resources;
dispose(Owner, [Key|T], Resources) ->
    {Res, Dtor, Pids} = gb_trees:get(Key, Resources),
    NewPids = gb_sets:delete(Owner, Pids),
    case gb_sets:is_empty(NewPids) of
        true ->
            ok = Dtor(Res),
            gb_trees:delete(Key, Resources);
        false ->
            gb_trees:update(Key, {Res, Dtor, NewPids}, Resources)
    end.

-spec dispose_all(none | {key(), {resource(), dtor(), gb_set()}, gb_trees:iter()} |
                  resources()) -> ok.
%% @private
dispose_all(none) ->
    ok;
dispose_all({_Key, {Res, Dtor, Pids}, Iter}) ->
    _True = [demonitor(P) || P <- gb_sets:to_list(Pids)],
    ok = Dtor(Res),
    dispose_all(gb_trees:next(Iter));
dispose_all(Resources) ->
    dispose_all(gb_trees:next(gb_trees:iterator(Resources))).

