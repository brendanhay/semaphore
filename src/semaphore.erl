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

%%
%% API
%%

-spec checkout(key(), ctor(), dtor()) -> resource().
%% @doc
checkout(Key, Ctor, Dtor) ->
    gen_server:call(?SERVER, {checkout, Key, Ctor, Dtor}).

-spec info() -> {[lock()], [{key(), resource()}]}.
%% @doc
info() -> gen_server:call(?SERVER, info).

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
    {ok, Pid} = semaphore_sup:start_link().

-spec stop(_) -> ok.
%% @hidden
stop(_State) -> ok.
