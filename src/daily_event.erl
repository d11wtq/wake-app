%% @doc Runs a command at the same time every day.

-module(daily_event).

-export([start/3,
         start_link/3]).

-record(event, {cmd,
                start_at,
                end_at,
                state=wait}).

%% -- public api

%% @doc Start the event server standalone.
%%
%% @spec start(Cmd, {H1,M1,S1}, {H2,M2,S2}) -> {ok, Pid}.
%% @spec start("say 'Wakey Wakey!'", {6,0,0}, {6,10,0}) -> {ok, Pid}.
start(Cmd, Start = {_, _, _}, End = {_, _, _}) ->
  {ok, spawn(fun() -> loop(make_event(Cmd, Start, End)) end)}.

%% @doc Start the event server.
%%
%% @spec start_link(Cmd, {H1,M1,S1}, {H2,M2,S2}) -> {ok, Pid}.
%% @spec start_link("say 'Wakey Wakey!'", {6,0,0}, {6,10,0}) -> {ok, Pid}.
start_link(Cmd, Start = {_, _, _}, End = {_, _, _}) ->
  {ok, spawn_link(fun() -> loop(make_event(Cmd, Start, End)) end)}.

%% -- private api

loop(Event = #event{state=wait}) ->
  case within_run_window(calendar:local_time(), Event) of
    true ->
      io:format("Running `~s'~n", [Event#event.cmd]),
      os:cmd(Event#event.cmd),
      loop(Event#event{state=running});
    false ->
      timer:sleep(1000),
      loop(Event)
  end;
loop(Event = #event{state=running}) ->
  case within_run_window(calendar:local_time(), Event) of
    true ->
      timer:sleep(1000),
      loop(Event);
    false ->
      io:format("Stopping `~s'~n", [Event#event.cmd]),
      loop(Event#event{state=wait})
  end.

make_event(Cmd, {H1,M1,S1}, {H2,M2,S2}) ->
  #event{cmd=Cmd, state=wait, start_at={H1,M1,S1}, end_at={H2,M2,S2}}.

within_run_window({Date, Time}, #event{start_at={H1,M1,S1}, end_at={H2,M2,S2}}) ->
  Now   = calendar:datetime_to_gregorian_seconds({Date, Time}),
  Start = calendar:datetime_to_gregorian_seconds({Date, {H1, M1, S1}}),
  End   = calendar:datetime_to_gregorian_seconds({Date, {H2, M2, S2}}),
  Now >= Start andalso Now < End.
