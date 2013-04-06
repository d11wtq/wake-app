%% @doc Supervisor for the main scheduler process.

-module(wake_app_scheduler_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

%% @doc Start the supervisor, registering the scheduler with Name.
%%
%% @spec start_link(RegName) -> {ok, pid()} | {error, Reason}.
start_link(Name) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Name).

%% -- supervisor callback

init(Name) ->
  {ok, {{one_for_one, 60, 3600},
        [{wake_app_scheduler,
          {wake_app_scheduler, start_link, [Name]},
          transient,
          3000,
          worker,
          [wake_app_scheduler]}]}}.
