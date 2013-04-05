%% @doc Maintains a list of scheduled tasks.

-module(wake_app_scheduler).
-behaviour(gen_server).

-export([start_link/0,
         stop/1,
         schedule/2,
         unschedule/2,
         list/1]).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% -- public api

%% @doc Start the schedule server running.
%%
%% @spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, dict:new(), []).

%% @doc Add a task to the schedule.
%%
%% @spec schedule(Pid, Cmd, Start, Duration) -> {ok, ref()} | {error, Reason}.
schedule(Pid, {Cmd, Start, Duration}) ->
  gen_server:call(Pid, {schedule, Cmd, Start, Duration}).

%% @doc Remove a task from the schedule.
%%
%% @spec unschedule(Pid, TaskRef) -> ok | {error, not_found}.
unschedule(Pid, TaskRef) ->
  gen_server:call(Pid, {unschedule, TaskRef}).

%% @doc List all tasks on the schedule.
%%
%% @spec list(Pid) -> [{#Ref<0.0.0.38>,{"ls",{0,0,0},60}}].
list(Pid) ->
  gen_server:call(Pid, list).

%% @doc Shutdown the scheduler.
%%
%% @spec stop(Pid) -> ok.
stop(Pid) ->
  gen_server:cast(Pid, shutdown).

%% -- gen_server callbacks

%% Perform any custom initialization of the task list.
init(Tasks) ->
  {ok, Tasks}.

%% Nothing to do during termination.
terminate(normal, _) ->
  ok.

%% Add a new task to the schedule.
handle_call({schedule, Cmd, Start, Duration}, _From, Tasks) ->
  case wake_app_task:start_link(Cmd, Start, Duration) of
    {ok, Pid} ->
      TaskRef = make_ref(),
      {reply, {ok, TaskRef}, dict:store(TaskRef, Pid, Tasks)};
    {error, Reason} ->
      {reply, {error, Reason}, Tasks}
  end;

%% Remove a task from the schedule.
handle_call({unschedule, TaskRef}, _From, Tasks) ->
  case dict:find(TaskRef, Tasks) of
    {ok, Pid} ->
      exit(Pid, normal),
      {reply, ok, dict:erase(TaskRef, Tasks)};
    error ->
      {reply, {error, not_found}, Tasks}
  end;

%% List everything on the schedule.
handle_call(list, _From, Tasks) ->
  list_tasks(Tasks),
  {reply, ok, Tasks}.

%% Shutdown the server.
handle_cast(shutdown, Tasks) ->
  {stop, normal, ok, Tasks}.

handle_info(_Msg, Tasks) ->
  {noreply, Tasks}.

code_change(_OldVsn, Tasks, _Extra) ->
  {ok, Tasks}.

%% -- private api

list_tasks(Tasks) ->
  io:format("Scheduled Tasks:~n"),
  dict:fold(fun(TaskRef, Pid, _) ->
              io:format("~p: ~p~n", [TaskRef, Pid])
            end, [], Tasks).
