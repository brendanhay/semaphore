%% @doc
-module(semaphore_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/semaphore.hrl").

-define(TIMEOUT, 10000).

-type state() :: {[lock()], [{key(), resource(), dtor()}]}.
-type call()  :: {checkout, key(), ctor(), dtor()} | info.

%%
%% API
%%

-spec start_link(atom()) -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {[], []}}.
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, {[], []}}.

-spec handle_call(call(), _, state()) -> {reply, any(), state()}.
%% @hidden
handle_call({checkout, Key, Ctor, Dtor}, {Pid, _Tag}, State) ->
    {Res, NewState} = checkout(Key, Pid, Ctor, Dtor, State),
    {reply, Res, NewState};
handle_call(info, _From, State) ->
    {reply, State, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info({'DOWN', reference(), process, _, _}, state()) -> {noreply, state()}.
%% @hidden
handle_info({'DOWN', Ref, process, From, _Reason}, State) ->
    {noreply, checkin(Ref, From, State)}.

-spec terminate(_, state()) -> ok.
%% @hidden
terminate(_Reason, State) ->
    true = checkin_all(State),
    ok.

-spec code_change(_, state(), _) -> {ok, state()}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec checkout(any(), pid(), ctor(), dtor(), state()) -> {any(), state()}.
%% @private Find or instantiate a resource
checkout(Key, Pid, Ctor, Dtor, {Locks, Resources}) ->
    NewLocks = lock(Key, Pid, Locks),
    case find_resource(Key, Resources) of
        false ->
            try
                Res = Ctor(),
                NewResources = lists:keystore(Key, 1, Resources, {Key, Res, Dtor}),
                {Res, {NewLocks, NewResources}}
            catch
                error:Reason ->
                    {Reason, {Locks, Resources}}
            end;
        {Key, Res, _Dtor} ->
            {Res, {NewLocks, Resources}}
    end.

-spec checkin(reference(), pid(), state()) -> state().
%% @private
checkin(Ref, Pid, {Locks, Resources}) ->
    case unlock(Ref, Pid, Locks) of
        {false, Locks}  -> {Locks, Resources};
        {Key, NewLocks} -> {NewLocks, free_resource(Key, NewLocks, Resources)}
    end.

-spec checkin_all(state()) -> true | false.
%% @private
checkin_all({_Locks, Resources}) ->
    lists:all(fun(D) -> D =:= ok end,
              [Dtor(Res) || {_Key, Res, Dtor} <- Resources]).

-spec free_resource(any(), [lock()], [resource()]) -> any().
%% @private
free_resource(Key, Locks, Resources) ->
    case lists:keymember(Key, 2, Locks) of
        true ->
            Resources;
        false ->
            {Key, Res, Dtor} = find_resource(Key, Resources),
            ok = Dtor(Res),
            lists:keydelete(Key, 1, Resources)
    end.

-spec find_resource(any(), [resource()]) -> false | tuple().
%% @private
find_resource(Key, Resources) -> lists:keyfind(Key, 1, Resources).

-spec lock(any(), pid(), [lock()]) -> [lock()].
%% @private Add a {Key, Pid} tuple to owners
lock(Key, Pid, Locks) ->
    monitor(process, Pid),
    [{Pid, Key}|Locks].

-spec unlock(reference(), pid(), [lock()]) -> {any(), [lock()]}.
unlock(Ref, Pid, Locks) ->
    demonitor(Ref),
    case lists:keyfind(Pid, 1, Locks) of
        {Pid, Key} -> {Key, lists:keydelete(Pid, 1, Locks)};
        false      -> {false, Locks}
    end.
