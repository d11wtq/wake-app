# Simple task scheduler written in Erlang

This is a toy, written as part of my Erlang learning and not something I
expect other people to use, or find interesting.

It starts one or more task schedulers that run an arbitrary shell command
at the same time every day.

I currently use it to start the radio playing for an hour each morning.

## Example

``` erlang
% Start the scheduler
application:start(wake_app).

% Start playing SBS Radio 2 at 8am every morning, until 9am.
{ok, TaskRef} = wake_app:schedule("open 'radium://tune-in/?h=&b=sbs&c=2&'",
                                  {8,0,0},
                                  3600).

% Stop the scheduler
application:stop(wake_app).
```

## License & Copyright

Haha.
