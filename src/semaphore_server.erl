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
    {Res, NewState} = checkout(Key, Pid, Ctor, Dtor, State),
    {reply, Res, NewState};
handle_call(info, _From, State) ->
    %% Create a proplist of {pid, key, [resources]} for each pid
    {reply, State, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info({'DOWN', reference(), process, _, _}, state()) -> {noreply, state()}.
%% @hidden
handle_info({'DOWN', Ref, process, _From, _Reason}, State) ->
    {noreply, checkin(Ref, State)}.

-spec terminate(_, state()) -> ok.
%% @hidden
terminate(_Reason, State) ->
    release_unused(State),
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
                    %% Register an aggregated counter for this key
                    true = gproc:reg(?AGGR(Key)),
                    try
                        Res = Ctor(),
                        {Res, gb_trees:insert(Key, {Res, Dtor}, Resources)}
                    catch
                        error:Reason ->
                            {Reason, Resources}
                    end
            end,
    %% Monitor Owner's 'DOWN' messages to automate checkin
    monitor(process, Owner),
    %% !: Make this safe
    %% Register a local counter for this key
    true = gproc:reg(?CNTR(Key)),
    %% And hand it off the calling process
    Owner = gproc:give_away(?CNTR(Key), Owner),
    Reply.

-spec checkin(reference(), state()) -> state().
%% @doc !: Must be a better way to do this than iterating
%% over every element
checkin(Ref, Resources) ->
    demonitor(Ref),
    release_unused(Resources).

-spec release_unused(state()) -> state().
%% @doc !: Must be a better way to do this than iterating
%% over every element
release_unused(Resources) ->
    release_unused(gb_trees:iterator(Resources), Resources).

-spec release_unused(none | {key(), {resource(), dtor()}, gb_trees:iter()},
                     state()) -> state().
%% @private
release_unused(none, Resources) ->
    Resources;
release_unused({Key, {_Res, Dtor}, NextIter}, Resources) ->
    release_unused(gb_trees:next(NextIter),
            case try_release(Key, Dtor) of
                true  -> gb_trees:delete(Key, Resources);
                false -> Resources
            end).

-spec try_release(key(), dtor()) -> true | false.
%% @private
try_release(Key, Dtor) ->
    %% Check the aggregated counter
    case gproc:lookup_value(?AGGR(Key)) of
        0 ->
            %% No clients, release
            ok = Dtor(),
            %% Remove gproc aggregated counter
            gproc:unreg(?AGGR(Key));
        _N ->
            false
    end.
