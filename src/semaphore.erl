%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

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

-spec checkout(key(), ctor(), dtor()) -> any().
%% @doc
checkout(Key, Ctor, Dtor) ->
    gen_server:call(?SERVER, {checkout, Key, Ctor, Dtor}, ?TIMEOUT).

-spec info() -> [{pid(), [key()]}].
%% @doc
info() -> gen_server:call(?SERVER, info, ?TIMEOUT).

-spec start() ->  ok | {error, _}.
%% @doc
start() -> application:start(?MODULE, permanent).

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
