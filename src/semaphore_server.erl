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
-type mode()      :: unused | force.
-type resources() :: gb_tree(). %% gb_tree(key(), {resource(), dtor()})

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, gb_tree()}.
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, gb_trees:empty()}.

-spec handle_call(call(), _, resources()) -> {reply, any(), resources()}.
%% @hidden
handle_call({checkout, Key, Ctor, Dtor}, {Pid, _Tag}, Resources) ->
    {Reply, NewResources} = checkout(Key, Ctor, Dtor, Resources),
    ok = add_counter(Pid, Key),
    {reply, Reply, NewResources};
handle_call(info, _From, Resources) ->
    {reply, info(Resources), Resources}.

-spec handle_cast(any(), resources()) -> {noreply, resources()}.
%% @hidden
handle_cast(_Msg, Resources) -> {noreply, Resources}.

-spec handle_info({'DOWN', reference(), process, _, _}, resources()) -> {noreply, resources()}.
%% @hidden
handle_info({'DOWN', _Ref, process, _From, _Reason}, Resources) ->
    {noreply, checkin(Resources, unused)}.

-spec terminate(_, resources()) -> ok.
%% @hidden
terminate(_Reason, Resources) ->
    _NewResources = checkin(Resources, force),
    ok.

-spec code_change(_, resources(), _) -> {ok, resources()}.
%% @hidden
code_change(_OldVsn, Resources, _Extra) -> {ok, Resources}.

%%
%% Private
%%

-spec info(resources()) -> [{key(), [pid()]}].
%% @private
info(Resources) ->
    [{K, R, gproc:lookup_pids(?CNTR(K))} ||
        {K, {R, _D}} <- gb_trees:to_list(Resources)].

-spec add_counter(pid(), key()) -> ok.
%% @private
add_counter(Owner, Key) ->
    %% !: (efficiency) Check if Owner already has a counter
    case lists:member(Owner, gproc:lookup_pids(?CNTR(Key))) of
        true ->
            ok;
        false ->
            %% Monitor Owner's 'DOWN' messages to automate checkin
            _Ref = monitor(process, Owner),
            %% Add a counter of 1
            true = gproc:reg(?CNTR(Key), 1),
            %% Transfer the counter to Owner
            Owner = gproc:give_away(?CNTR(Key), Owner),
            ok
    end.

-spec checkout(key(), ctor(), dtor(), resources()) -> {resource(), resources()}.
%% @private Find or instantiate a resource
checkout(Key, Ctor, Dtor, Resources) ->
    %% Check if resources already contains the key
    case gb_trees:lookup(Key, Resources) of
        {value, {Res, _Dtor}} ->
            {Res, Resources};
        none ->
            %% Register an aggregate counter for this key
            true = gproc:reg(?AGGR(Key)),
            Res = Ctor(),
            {Res, gb_trees:insert(Key, {Res, Dtor}, Resources)}
    end.

-spec checkin(resources(), mode()) -> resources().
%% @doc !: Must be a better way to do this than iterating
%% over every element
checkin(Resources, Mode) ->
    Iter = gb_trees:iterator(Resources),
    checkin(gb_trees:next(Iter), Resources, Mode).

-spec checkin(none | {key(), {resource(), dtor()}, gb_trees:iter()},
              resources(), mode()) -> resources().
%% @private
checkin(none, Resources, _Mode) ->
    Resources;
checkin({Key, Value, NextIter}, Resources, Mode) ->
    checkin(gb_trees:next(NextIter),
            case dispose(Key, Value, Mode) of
                true  -> gb_trees:delete(Key, Resources);
                false -> Resources
            end,
            Mode).

-spec dispose(key(), {resource(), dtor()}, mode()) -> true | false.
%% @private
dispose(Key, {Res, Dtor}, force) ->
    ok = Dtor(Res),
    gproc:unreg(?AGGR(Key));
dispose(Key, Value, unused) ->
    case gproc:lookup_value(?AGGR(Key)) of
        0  -> dispose(Key, Value, force);
        _N -> false
    end.
