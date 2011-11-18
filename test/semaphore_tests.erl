%% @doc
-module(semaphore_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/semaphore.hrl").

start_child(Fun) ->
    proc_lib:start_link(?MODULE, init_child, [self(), Fun]).

init_child(Parent, Fun) ->
    proc_lib:init_ack(Parent, self()),
    Res = Fun(),
    receive stop -> Parent ! Res end.

stop_child(Pid) ->
    Pid ! stop,
    receive Any -> Any end.

setup() ->
    semaphore:start(),
    Key = key,
    Res = resource,
    Self = self(),
    Dtor = fun(X) -> Self ! X, timer:sleep(2), error(process_info(Self, messages)) end,
    Checkout = fun() -> semaphore:checkout(Key, fun() -> Res end, Dtor) end,
    {Key, Res, Checkout, Dtor}.

teardown(_Setup) ->
    semaphore:stop(),
    ok.

semaphore_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun app_already_started/0,
      fun supervisor_registered/0,
      fun server_registered/0,
      fun server_traps_exits/0,
      fun server_initial_state/0,
      fun checkout_resource/0,
      fun release_resource/0
     ]}.

app_already_started() ->
    ?assertEqual({error, {already_started, semaphore}}, semaphore:start()).

supervisor_registered() ->
    Sup = whereis(semaphore_sup),
    ?assertNotEqual(undefined, Sup),
    ?assert(is_pid(Sup)).

server_registered() ->
    Srv = whereis(?SERVER),
    ?assertNotEqual(undefined, Srv),
    ?assert(is_pid(Srv)).

server_traps_exits() ->
    Srv = whereis(?SERVER),
    ?assertEqual({trap_exit, true}, process_info(Srv, trap_exit)).

server_initial_state() ->
    ?assertEqual({[], []}, semaphore:info()).

checkout_resource() ->
    {Key, Res, Self} = {key, resource, self()},
    {Ctor, Dtor} = {fun() -> Res end, fun(X) -> Self ! X end},
    Fun = fun() -> semaphore:checkout(Key, Ctor, Dtor) end,

    %% Fun from child A
    A = start_child(Fun),
    ?assertEqual({[{A, Key}], [{Key, Res, Dtor}]},  semaphore:info()),

    %% Fun from child B
    B = start_child(Fun),
    ?assertEqual({[{B, Key}, {A, Key}], [{Key, Res, Dtor}]}, semaphore:info()),

    %% Remove Child A's lock
    ?assertEqual(Res, stop_child(A)),
    ?assertEqual({[{B, Key}], [{Key, Res, Dtor}]}, semaphore:info()),

    %% Remove Child B's lock
    ?assertEqual(Res, stop_child(B)),
    ?assertEqual({[], []}, semaphore:info()).

release_resource() ->
    {Key, Res, Self} = {key, resource, self()},
    {Ctor, Dtor} = {fun() -> Res end, fun(X) -> Self ! X end},
    Fun = fun() -> semaphore:checkout(Key, Ctor, Dtor) end,

    %% Spin up 5 children and checkout the resource
    Pids = [start_child(Fun) || _N <- lists:seq(1, 5)],

    %% Check the locks match the 5 children, and there's only 1 resource
    {Locks, Resources} = semaphore:info(),
    ?assertEqual(length(Pids), length(Locks)),
    ?assertEqual(1, length(Resources)),

    %% Hard stop and check the resource is released
    semaphore:stop(),

    ?assert(receive Res -> true
            after   100 -> false
            end).
