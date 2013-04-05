%% @doc Runs a command at the same time every day.

-module(wake_app_task).

-export([start/3,
         start_link/3]).

-record(task, {cmd,
               start_at,
               duration=60,
               state=waiting}).

%% -- public api

%% @doc Start the task server standalone.
%%
%% @spec start(Cmd, {H,M,S}, Duration) -> {ok, Pid}.
%% @spec start("say 'Wakey Wakey!'", {6,0,0}, 30) -> {ok, Pid}.
start(Cmd, Start = {_, _, _}, Duration) ->
  {ok, spawn(fun() -> loop(make_task(Cmd, Start, Duration)) end)}.

%% @doc Start the task server.
%%
%% Cmd is the shell command to run.
%% {H,M,S} represents the time at which it should run.
%% Duration is the number of seconds the task should run for.
%%
%% @spec start_link(Cmd, {H,M,S}, Duration) -> {ok, Pid}.
%% @spec start_link("say 'Wakey Wakey!'", {6,0,0}, 30) -> {ok, Pid}.
start_link(Cmd, Start = {_, _, _}, Duration) ->
  {ok, spawn_link(fun() -> loop(make_task(Cmd, Start, Duration)) end)}.

%% -- private api

loop(Task = #task{state=waiting}) ->
  case within_run_window(calendar:local_time(), Task) of
    true ->
      io:format("Running `~s'~n", [Task#task.cmd]),
      os:cmd(Task#task.cmd),
      loop(Task#task{state=running});
    false ->
      timer:sleep(1000),
      loop(Task)
  end;
loop(Task = #task{state=running}) ->
  case within_run_window(calendar:local_time(), Task) of
    true ->
      timer:sleep(1000),
      loop(Task);
    false ->
      io:format("Stopping `~s'~n", [Task#task.cmd]),
      loop(Task#task{state=waiting})
  end.

make_task(Cmd, {H,M,S}, Duration) ->
  #task{cmd=Cmd, state=waiting, start_at={H,M,S}, duration=Duration}.

within_run_window({Date, Time}, #task{start_at={H, M, S}, duration=Duration}) ->
  Now   = calendar:datetime_to_gregorian_seconds({Date, Time}),
  Start = calendar:datetime_to_gregorian_seconds({Date, {H, M, S}}),
  Now >= Start andalso Now < Start + Duration.
