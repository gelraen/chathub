-module(chathub).
-behaviour(supervisor).
-behaviour(application).

-export([start/2, init/1, stop/1]).

start(_StartType, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, true).

stop(_) ->
    ok.

init(true) ->
    Config = load_config(),
    Childs = start_hubs(Config),
    {ok, {{one_for_one, 10, 3}, Childs}}.

load_config() ->
    [].

start_hubs(Config) ->
    [].
