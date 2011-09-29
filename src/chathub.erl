-module(chathub).
-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, true).

init(true) ->
    Config = load_config(),
    Childs = start_hubs(Config),
    {ok, {{one_for_one, 10, 3}, Childs}}.

load_config() ->
    [].

start_hubs(Config) ->
    [].
