-module(osc_util).

-export([when_to_millisecs/1,
         log_data/1]).

%% @doc Converts OSC time to milliseconds.
%% @spec when_to_millisecs(When) -> integer()
%%       When = immediately | {time, Seconds::integer(), Fractions::integer()}
when_to_millisecs(immediately) ->
    0;
when_to_millisecs({time, Seconds, Fractions}) ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    S = (Seconds - 2208988800) - (MegaSecs * 1000000 + Secs),
    F = Fractions - (MicroSecs * 1000000),
    case (S * 1000) + (1000 div F) of
	Time when Time > 0 ->
	    Time;
	_ ->
	    0
    end.

%% @doc Logs handled data.
%% @spec log_data(Data) -> Data
log_data(Data) ->
    error_logger:info_msg("Received message: ~p", [Data]),
    Data.
