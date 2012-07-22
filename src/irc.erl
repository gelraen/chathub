-module(irc).
-behaviour(gen_server).
-behaviour(proto_handler).

-export([start/2, send_msg/2, user_added/3, user_removed/2, user_renamed/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([join/3, part/3, message/3, private_message/4, nick_change/3]).

-include("hub.hrl").

-record(state, {parent, host, port, channel, users = []}).

% irc-specific callbacks
join(Pid, Nick, Data) ->
	gen_server:cast(Pid, {join, Nick, Data}).

part(Pid, Nick, Data) ->
	gen_server:cast(Pid, {part, Nick, Data}).

message(Pid, FromNick, Text) ->
	gen_server:cast(Pid, {message, FromNick, Text}).

private_message(Pid, FromNick, ToNick, Text) ->
	gen_server:cast(Pid, {private_message, FromNick, ToNick, Text}).

nick_change(Pid, OldNick, NewNick) ->
	gen_server:cast(Pid, {nick_change, OldNick, NewNick}).

% proto_handler callbacks
start(Config, Parent) ->
	gen_server:start_link(irc, {Config, Parent}, []).

send_msg(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

user_added(Pid, Id, Name) ->
	gen_server:cast(Pid, {add_user, Id, Name}).

user_removed(Pid, Id) ->
	gen_server:cast(Pid, {remove_user, Id}).

user_renamed(Pid, Id, NewName) ->
	gen_server:cast(Pid, {rename_user, Id, NewName}).

% gen_server callbacks
init({{Host, Port, Channel}, Parent}) ->
	gen_server:start_link(irc_client, {self(), Host, Port, Channel, "chathub", "Test Test", "chathub"}, []),
	{ok, #state{parent=Parent, host=Host, port=Port, channel=Channel}}.

handle_call(Request, Parent, #state{parent=Parent} = State) ->
	{noreply, State};

handle_call(Request, From, State) ->
	{noreply, State}.

handle_cast({join, Nick, _Data}, State) ->
	{ok, Id} = hub:add_user(State#state.parent, Nick),
	NewUsers = State#state.users ++ [{Id, Nick}],
	{noreply, State#state{users = NewUsers}};

handle_cast({part, Nick, _Data}, State) ->
	NewUsers = case lists:keyfind(Nick, 2, State#state.users) of
	{Id, Nick} = El ->
		hub:remove_user(State#state.parent, Id),
		lists:delete(El, State#state.users);
	false ->
		State#state.users
	end,
	{noreply, State#state{users = NewUsers}};

handle_cast({nick_change, OldNick, NewNick}, State) ->
	NewUsers = case lists:keyfind(OldNick, 2, State#state.users) of
	{Id, OldNick} = El ->
		hub:rename_user(State#state.parent, Id, NewNick),
		lists:keyreplace(Id, 1, State#state.users, {Id, NewNick});
	false ->
		State#state.users
	end,
	{noreply, State#state{users = NewUsers}};

handle_cast({message, Nick, Text}, State) ->
	{Id, Nick} = lists:keyfind(Nick, 2, State#state.users),
	hub:send_msg(State#state.parent, #msg{from = Id, body = Text}),
	{noreply, State};

handle_cast(Request, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	{noreply, State}.

terminate(Reason, State) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.
