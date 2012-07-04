-module(echo).
-behaviour(gen_server).
-behaviour(proto_handler).

-export([start/2, send_msg/2, user_added/3, user_removed/2, user_renamed/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {parent, name}).

start(Config, Parent) ->
	gen_server:start_link(echo, {Config, Parent}, []).

send_msg(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

user_added(Pid, Id, Name) ->
	gen_server:cast(Pid, {add_user, Id, Name}).

user_removed(Pid, Id) ->
	gen_server:cast(Pid, {remove_user, Id}).

user_renamed(Pid, Id, NewName) ->
	gen_server:cast(Pid, {rename_user, Id, NewName}).

init({Name, Parent}) ->
	io:format("~s:Starting echo handler with pid ~p~n", [Name, self()]),
	{ok, #state{parent=Parent,name=Name}}.

handle_call(Request, Parent, #state{parent=Parent} = State) ->
	io:format("~s:Sync request: ~p~n", [State#state.name, Request]),
	{noreply, State};

handle_call(Request, From, State) ->
	io:format("~s:Sync request from ~p: ~p~n", [State#state.name, From, Request]),
	{noreply, State}.

handle_cast(Request, State) ->
	io:format("~s:Async request: ~p~n", [State#state.name, Request]),
	{noreply, State}.

handle_info(Info, State) ->
	io:format("~s:Info received: ~p~n", [State#state.name, Info]),
	{noreply, State}.

terminate(Reason, State) ->
	io:format("~s:Shutting down with reason: ~p~n", [State#state.name, Reason]).

code_change(_, State, _) ->
	{ok, State}.
