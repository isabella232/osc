%% @author Tobias Rodaebel
%% @doc Adding OSC methods.

-module(osc_methods).

%% API
-export([
     add_methods/0, add_methods/1,
     get_methods/0,
     remove_methods/0]).

%% Debug API
-export([ping/0]).

%% Constants
-define(SERVER, {global, osc_server}).
-define(DEFAULT_METHODS, [
    {add_method, "/1/stop", osc_util, log_data}
]).

%% @doc Adds methods.
%% @spec add_methods() -> [ok, ...]
add_methods() ->
    add_methods(?DEFAULT_METHODS).

add_methods(Methods) ->
    [gen_server:cast(?SERVER, X) || X <- Methods].

%% @doc Get methods.
%% @spec get_methods() -> ["...", ...]
get_methods() ->
    gen_server:call(?SERVER, get_methods).

%% @doc Removes methods.
%% @spec remove_methods() -> [ok, ...]
remove_methods() ->
    remove_methods(get_methods()).

remove_methods(Methods) ->
    [gen_server:cast(?SERVER, {remove_method, X}) || X <- Methods].

%% Debgging utility calls
ping() ->
    gen_server:call(?SERVER, ping).
