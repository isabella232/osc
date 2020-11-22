-module(osc).

%% API
-export([
     add_addresses/0, add_addresses/1,
     get_addresses/0,
     remove_addresses/0]).

%% Debug API
-export([ping/0]).

%% Constants
-define(SERVER, {global, osc_server}).
-define(DEFAULT_METHODS, [
    {add_address, "/debug/log_message", osc_util, log_data}
]).

%% @doc Adds OSC addresses.
%% @spec add_addresses() -> [ok, ...]
add_addresses() ->
    add_addresses(?DEFAULT_METHODS).

add_addresses(Methods) ->
    [gen_server:cast(?SERVER, X) || X <- Methods].

%% @doc Get OSC addresses.
%% @spec get_addresses() -> ["...", ...]
get_addresses() ->
    gen_server:call(?SERVER, get_addresses).

%% @doc Removes OSC addresses.
%% @spec remove_addresses() -> [ok, ...]
remove_addresses() ->
    remove_addresses(get_addresses()).

remove_addresses(Methods) ->
    [gen_server:cast(?SERVER, {remove_address, X}) || X <- Methods].

%% Debgging utility calls
ping() ->
    gen_server:call(?SERVER, ping).
