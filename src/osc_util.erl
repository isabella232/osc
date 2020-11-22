-module(osc_util).

-export([log_data/1]).

%% @doc Logs handled data.
%% @spec log_data(Data) -> Data
log_data(Data) ->
    error_logger:info_msg("Received message: ~p", [Data]),
    Data.
