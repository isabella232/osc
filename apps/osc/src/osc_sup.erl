%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc OSC Supervisor

-module(osc_sup).
-author('ruslan@babayev.com').

-behaviour(supervisor).

-define(SERVER, osc_server). 

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes the supervisor.
%% @spec init(Args) -> {ok, {SupFlags, ChildSpecs}} | ignore | {error, Reason}
init([]) ->
    Server = #{
        id => ?SERVER,
        start => {?SERVER, start_link, []},
	restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [?SERVER]},
    {ok, {#{
          strategy => one_for_one,
          intensity => 3,
          period => 60}, [Server]}}.
