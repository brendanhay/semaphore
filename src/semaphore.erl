%% @doc
-module(semaphore).
-behaviour(application).

%% API
-export([checkout/3,
         info/0,
         start/0,
         stop/0]).

%% Callbacks
-export([start/2,
         stop/1]).

-include("include/semaphore.hrl").

-define(TIMEOUT, 10000).

%%
%% API
%%

-spec checkout(key(), ctor(), dtor()) -> resource().
%% @doc
checkout(Key, Ctor, Dtor) ->
    gen_server:call(?SERVER, {checkout, Key, Ctor, Dtor}, ?TIMEOUT).

-spec info() -> gb_tree().
%% @doc
info() -> gen_server:call(?SERVER, info, ?TIMEOUT).

-spec start() ->  ok | {error, _}.
%% @doc
start() -> application:start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() -> application:stop(?MODULE).

%%
%% Callbacks
%%

-spec start(normal, list()) -> {ok, pid()}.
%% @hidden
start(normal, _Args) ->
    {ok, _Pid} = semaphore_sup:start_link().

-spec stop(_) -> ok.
%% @hidden
stop(_State) -> ok.
