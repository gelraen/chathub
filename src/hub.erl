-module(hub).
-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start/1]).

-export([send_msg/2]).

-include("hub.hrl").

start({hub, Name, _, _} = Config) ->
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

send_msg(Msg, Name) ->
    gen_server:cast(Name, {msg, self(), Msg}).

init({hub, Name, Config, ChildConfig}) ->
    Children = lists:map(fun(X) ->
				spawn_child(X, Name)
			end,
		    ChildConfig),
    {ok, #state{children = Children, config = Config}}.

spawn_child({Type, Args} = Config, _ParentName) ->
    Pid = Type:start(Args),
    {Pid, Config}.

handle_call(_, _From, State) ->
    {reply, ignored, State}.

handle_cast({msg, From, Msg}, State) ->
    lists:foreach(fun ({Pid, {Type, _}}) ->
    			if
	    		From == Pid ->
				ok;
			true ->		    
				Type:send_msg(Pid, {msg, Msg})
			end
		end, State#state.children),
    {reply, ok, State};
handle_cast(_, State) ->
    {reply, ignored, State}.

handle_info(_, State) ->
    {reply, ignored, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.


