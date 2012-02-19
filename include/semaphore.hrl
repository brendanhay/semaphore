%%
%% Registered
%%

-define(SERVER, semaphore_server).

%%
%% Types
%%

-type key()      :: any().
-type resource() :: any().
-type ctor()     :: fun(() -> resource()).
-type dtor()     :: fun((resource()) -> ok).

%%
%% Macros
%%

-define(CNTR(Name), {c, l, {?SERVER, Name}}).
-define(AGGR(Name), {a, l, {?SERVER, Name}}).

