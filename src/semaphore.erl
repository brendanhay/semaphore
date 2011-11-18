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

%% @doc
checkout(Key, Ctor, Dtor) -> gen_server:call(?SERVER, {checkout, Key, Ctor, Dtor}).

%% @doc
info() -> gen_server:call(?SERVER, info).

%% @doc
start() -> application:start(?MODULE).

%% @doc
stop() -> application:stop(?MODULE).

%%
%% Callbacks
%%

-spec start(normal, list()) -> ignore | {error, _} | {ok, pid()}.
%% @hidden
start(normal, _Args) -> semaphore_sup:start_link().

-spec stop(_) -> ok.
%% @hidden
stop(_State) -> ok.
