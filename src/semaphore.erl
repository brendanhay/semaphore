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
start() -> start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() -> application:stop(?MODULE).

%%
%% Callbacks
%%

-spec start(normal, list()) -> {ok, pid()}.
%% @hidden
start(normal, _Args) -> semaphore_sup:start_link().

-spec stop(_) -> ok.
%% @hidden
stop(_State) -> ok.

%%
%% Dependencies
%%

-spec start(atom()) -> ok.
%% @private
start(App) -> ensure_started(App, application:start(App, permanent)).

-spec ensure_started(atom(), ok | {error, {already_started, atom()} | {not_started, atom()}}) -> ok.
%% @private
ensure_started(_App, ok) ->
    ok;
ensure_started(_App, {error, {already_started, _App}}) ->
    ok;
ensure_started(App, {error, {not_started, Dep}}) ->
    start(Dep),
    start(App);
ensure_started(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

