%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

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

-type locks()     :: [{pid(), key()}].
-type resources() :: [{key(), any(), dtor()}].
-type state()     :: {locks(), resources()}.

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {[], []}}.
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, {[], []}}.

-spec handle_call({checkout, any(), ctor(), dtor()} | info, _, state()) -> {reply, any(), state()}.
%% @hidden
handle_call({checkout, Key, Ctor, Dtor}, {Pid, _Tag}, State) ->
    {Res, NewState} = fetch_resource(Key, Pid, Ctor, Dtor, State),
    {reply, Res, NewState};
handle_call(info, _From, State) ->
    {reply, State, State}.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.
%% @hidden
handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_info({'DOWN', reference(), process, _, _}, state()) -> {noreply, state()}.
%% @hidden
handle_info({'DOWN', Ref, process, From, _Reason}, State) ->
    {noreply, release_resource(Ref, From, State)}.

-spec terminate(_, state()) -> ok.
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

-spec fetch_resource(any(), pid(), ctor(), dtor(), state()) -> {any(), state()}.
%% @private Find or instantiate a resource
fetch_resource(Key, Pid, Ctor, Dtor, {Locks, Resources}) ->
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

-spec release_resource(reference(), pid(), state()) -> state().
%% @private
release_resource(Ref, Pid, {Locks, Resources}) ->
    case unlock(Ref, Pid, Locks) of
        {false, Locks}  -> {Locks, Resources};
        {Key, NewLocks} -> {NewLocks, free_resource(Key, NewLocks, Resources)}
    end.

-spec release_all(state()) -> [any()].
%% @private
release_all({_Locks, Resources}) -> [Dtor(Res) || {_Key, Res, Dtor} <- Resources].

-spec free_resource(any(), locks(), resources()) -> any().
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

-spec find_resource(any(), resources()) -> false | tuple().
%% @private
find_resource(Key, Resources) -> lists:keyfind(Key, 1, Resources).

-spec lock(any(), pid(), locks()) -> locks().
%% @private Add a {Key, Pid} tuple to owners
lock(Key, Pid, Locks) ->
    monitor(process, Pid),
    [{Pid, Key}|Locks].

-spec unlock(reference(), pid(), locks()) -> {any(), locks()}.
%% @private Remove all instances of an owner
unlock(Ref, Pid, Locks) ->
    demonitor(Ref),
    case lists:keyfind(Pid, 1, Locks) of
        {Pid, Key} -> {Key, lists:keydelete(Pid, 1, Locks)};
        false      -> {false, Locks}
    end.
