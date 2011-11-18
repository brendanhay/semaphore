%% @doc
-module(semaphore_srv).
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

-type state() :: {[{pid(), any()}], [{any(), any(), fun()}]}.

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% Callbacks
%%

%% @private
init([]) ->
    process_flag(trap_exit, true),
    {ok, {[], []}}.

-spec handle_call(any(), {pid(), _}, state()) -> {reply, ok, state()}.
%% @hidden
handle_call({checkout, Key, Ctor, Dtor}, {Pid, _Tag}, State) ->
    {Res, NewState} = fetch_resource(Key, Pid, Ctor, Dtor, State),
    {reply, Res, NewState};
handle_call(info, _From, State) ->
    {reply, State, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
%% @hidden
handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
%% @hidden
handle_info({'DOWN', Ref, process, From, _Reason}, State) ->
    {noreply, release_resource(Ref, From, State)}.

-spec terminate(any(), state()) -> ok.
%% @hidden
terminate(_Reason, State) ->
    release_all(State),
    ok.

-spec code_change(_, state(), _) -> {ok, state()}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

%% @private Find or instantiate a resource
fetch_resource(Key, Pid, Ctor, Dtor, {Locks, Resources}) ->
    NewLocks = lock(Key, Pid, Locks),
    case find_resource(Key, Resources) of
        false ->
            Res = Ctor(),
            NewResources = lists:keystore(Key, 1, Resources, {Key, Res, Dtor}),
            {Res, {NewLocks, NewResources}};
        {Key, Res, _Dtor} ->
            {Res, {NewLocks, Resources}}
    end.

%% @private
release_resource(Ref, Pid, {Locks, Resources}) ->
    {Key, NewLocks} = unlock(Ref, Pid, Locks),
    {NewLocks, free_resource(Key, NewLocks, Resources)}.

%% @private
release_all({_Locks, Resources}) -> [Dtor(Res) || {_Key, Res, Dtor} <- Resources].

%% @private
free_resource(Key, Locks, Resources) ->
    case lists:keymember(Key, 2, Locks) of
        true ->
            Resources;
        false ->
            {Key, Res, Dtor} = find_resource(Key, Resources),
            Dtor(Res),
            lists:keydelete(Key, 1, Resources)
    end.

%% @private
find_resource(Key, Resources) -> lists:keyfind(Key, 1, Resources).

%% @private Add a {Key, Pid} tuple to owners
lock(Key, Pid, Locks) ->
    monitor(process, Pid),
    [{Pid, Key}|Locks].

%% @private Remove all instances of an owner
unlock(Ref, Pid, Locks) ->
    demonitor(Ref),
    {Pid, Key} = lists:keyfind(Pid, 1, Locks),
    {Key, lists:keydelete(Pid, 1, Locks)}.
