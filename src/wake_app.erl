%% @doc Main application behaviour for wake-app.

-module(wake_app).
-behaviour(application).

-export([start/2,
         stop/1]).

-export([schedule/3,
         unschedule/1,
         list/0]).

%% -- public api

%% @doc Add a task to the schedule.
%%
%% @spec schedule(Cmd, Start, Duration) -> {ok, ref()} | {error, Reason}.
schedule(Cmd, Start, Duration) ->
  wake_app_scheduler:schedule(pid(), {Cmd, Start, Duration}).

%% @doc Remove a task from the schedule.
%%
%% @spec unschedule(Pid, TaskRef) -> ok | {error, not_found}.
unschedule(TaskRef) ->
  wake_app_scheduler:unschedule(pid(), TaskRef).

%% @doc List all tasks on the schedule.
%%
%% @spec list(Pid) -> [{#Ref<0.0.0.38>,{"ls",{0,0,0},60}}].
list() ->
  wake_app_scheduler:list(pid()).

%% -- application callbacks

%% @doc Start the application.
%%
%% @spec start(normal, []) -> {ok, Pid}.
start(normal, _Args) ->
  case wake_app_scheduler:start_link() of
    {ok, Pid} ->
      register(?MODULE, Pid),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Shutdown the application.
%%
%% @spec stop(State) -> ok.
stop(_State) ->
  ok.

%% -- private api

%% @private
pid() ->
  whereis(?MODULE).
