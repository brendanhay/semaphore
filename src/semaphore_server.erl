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

-type call()  :: {checkout, key(), ctor(), dtor()} | info.
-type mode()  :: unused | force.
-type state() :: gb_tree(). %% gb_tree(key(), {resource(), dtor()})

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {Reply, NewState} =
        try
            checkout(Key, Pid, Ctor, Dtor, State)
        catch
            error:Reason -> {Reason, State}
        end,
    {reply, Reply, NewState};
handle_call(info, _From, State) ->
    %% !: Create a proplist of {pid, key, [resources]} for each pid
    {reply, State, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info({'DOWN', reference(), process, _, _}, state()) -> {noreply, state()}.
%% @hidden
handle_info({'DOWN', _Ref, process, _From, _Reason}, State) ->
    {noreply, checkin(State, unused)}.

-spec terminate(_, state()) -> ok.
%% @hidden
terminate(_Reason, State) ->
    _NewState = checkin(State, force),
    ok.

-spec code_change(_, state(), _) -> {ok, state()}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec checkout(pid(), key(), ctor(), dtor(), state()) -> {resource(), state()}.
%% @private Find or instantiate a resource
checkout(Owner, Key, Ctor, Dtor, Resources) ->
    %% Check if resources already contains the key
    Reply = case gb_trees:lookup(Key, Resources) of
                {value, {Res, _Dtor}} ->
                    {Res, Resources};
                none ->
                    %% Register an aggregate counter for this key
                    true = gproc:reg(?AGGR(Key)),
                    Res = Ctor(),
                    {Res, gb_trees:insert(Key, {Res, Dtor}, Resources)}
            end,
    %% !: Check Owner doesn't already have a gproc counter assigned
    true = gproc:reg(?CNTR(Key), 1),
    %% Pass the counter to the Owner
    Owner = gproc:give_away(?CNTR(Key), Owner),
    %% Monitor Owner's 'DOWN' messages to automate checkin
    monitor(process, Owner),
    Reply.

-spec checkin(state(), mode()) -> state().
%% @doc !: Must be a better way to do this than iterating
%% over every element
checkin(Resources, Mode) ->
    Iter = gb_trees:iterator(Resources),
    checkin(gb_trees:next(Iter), Resources, Mode).

-spec checkin(none | {key(), {resource(), dtor()}, gb_trees:iter()},
              state(), mode()) -> state().
%% @private
checkin(none, Resources, _Mode) ->
    Resources;
checkin({Key, {_Res, Dtor}, NextIter}, Resources, Mode) ->
    checkin(gb_trees:next(NextIter),
            case dispose(Key, Dtor, Mode) of
                true  -> gb_trees:delete(Key, Resources);
                false -> Resources
            end,
            Mode).

-spec dispose(key(), dtor(), mode()) -> true | false.
%% @private
dispose(Key, Dtor, force) ->
    ok = Dtor(),
    gproc:unreg(?AGGR(Key));
dispose(Key, Dtor, unused) ->
    case gproc:lookup_value(?AGGR(Key)) of
        0  -> dispose(Key, Dtor, force);
        _N -> false
    end.
