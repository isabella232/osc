%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev

-module(osc_util).
-author('ruslan@babayev.com').

-export([log_data/1]).

%% @doc Logs handled data.
%% @spec log_data(Data) -> Data
log_data(Data) ->
    error_logger:info_msg("Received ~p", [Data]),
    Data.
