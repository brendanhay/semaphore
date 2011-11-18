%% @doc
-module(semaphore_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-include("include/semaphore.hrl").

%%
%% API
%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

init([]) ->
    Child = {?SERVER, {?SERVER, start_link, []},
             permanent, 8000, worker, [?SERVER]},
    {ok, {{one_for_one, 0, 1}, [Child]}}.
