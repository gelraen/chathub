-module(dc).
-behaviour(gen_server).
-behaviour(proto_handler).

-export([start/2, send_msg/2, user_added/3, user_removed/2, user_renamed/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([join/3, part/3, message/3, private_message/4, nick_change/3]).

-include("hub.hrl").

-record(state, {parent,
				host,
				port,
				localusers = [],
				remoteusers = [],
				socket,
				nick = "chathub",
				sharesize = 0,
				description = "Multiprotocol chatroom hub"}).

% protocol-specific callbacks
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
	gen_server:start_link(dc, {Config, Parent}, []).

send_msg(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

user_added(Pid, Id, Name) ->
	gen_server:cast(Pid, {add_user, Id, Name}).

user_removed(Pid, Id) ->
	gen_server:cast(Pid, {remove_user, Id}).

user_renamed(Pid, Id, NewName) ->
	gen_server:cast(Pid, {rename_user, Id, NewName}).

% gen_server callbacks
init({{Host, Port, Opts}, Parent}) ->
	State = lists:foldl(fun ({nick, NewNick}, State = #state{}) ->
			State#state{nick = NewNick};
		({sharesize, Share}, State = #state{}) ->
			State#state{sharesize = Share};
		({description, Descr}, State = #state{}) ->
			State#state{description = Descr};
		(_, State) ->
			State
		end, #state{}, Opts),
	{ok, Socket} = dc_client:start_master(Host, Port, State#state.nick, State#state.sharesize, State#state.description),
	{ok, State#state{parent=Parent, host=Host, port=Port, socket = Socket}}.

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast({join, Nick, _Data}, State = #state{remoteusers = RemoteUsers}) ->
	case lists:keyfind(Nick, 2, RemoteUsers) of
	{_Id, Nick, _Pid} ->
		{noreply, State};
	_ ->
		{ok, Id} = hub:add_user(State#state.parent, Nick),
		NewUsers = State#state.localusers ++ [{Id, Nick}],
		{noreply, State#state{localusers = NewUsers}}
	end;

handle_cast({part, Nick, _Data}, State = #state{parent = Parent, localusers = Users, remoteusers = RemoteUsers}) ->
	case lists:keyfind(Nick, 2, RemoteUsers) of
	{_Id, Nick, _Pid} ->
		{noreply, State};
	_ ->
		NewUsers = case lists:keyfind(Nick, 2, Users) of
		{Id, Nick} = El ->
			hub:remove_user(Parent, Id),
			lists:delete(El, Users);
		false ->
			Users
		end,
		{noreply, State#state{localusers = NewUsers}}
	end;

handle_cast({nick_change, OldNick, NewNick}, State = #state{remoteusers = Users}) ->
	NewUsers = case lists:keyfind(OldNick, 2, Users) of
	{Id, OldNick, Pid} ->
		lists:keyreplace(OldNick, 2, Users, {Id, NewNick, Pid});
	_ ->
		Users
	end,
	{noreply, State#state{remoteusers = NewUsers}};

handle_cast({message, Nick, Text}, State = #state{localusers = Users, parent = Parent}) ->
	case lists:keyfind(Nick, 2, Users) of
	{Id, Nick} ->
		hub:send_msg(Parent, #msg{from = Id, body = Text});
	_ ->
		ok
	end,
	{noreply, State};

handle_cast({private_message, FromNick, ToNick, Text}, State = #state{localusers = Users, parent = Parent, remoteusers = RemoteUsers}) ->
	case lists:keyfind(FromNick, 2, Users) of
	{FromId, FromNick} ->
		case lists:keyfind(ToNick, 2, RemoteUsers) of
		{ToId, ToNick} ->
			hub:send_msg(Parent, #privmsg{from = FromId, to = ToId, body = Text});
		_ ->
			ok
		end;
	_ ->
		ok
	end,
	{noreply, State};

handle_cast(#msg{from = FromId, body = Text}, State = #state{remoteusers = RemoteUsers}) ->
	{FromId, _FromNick, Pid} =  lists:keyfind(FromId, 1, RemoteUsers),
	dc_client:send_msg(Pid, Text),
	{noreply, State};

handle_cast(#privmsg{from = FromId, to = ToId, body = Text}, State = #state{remoteusers = RemoteUsers, localusers = Users}) ->
	{FromId, _FromNick, Pid} = lists:keyfind(FromId, 1, RemoteUsers),
	{ToId, ToNick} = lists:keyfind(ToId, 1, Users),
	dc_client:send_privmsg(Pid, ToNick, Text),
	{noreply, State};

handle_cast({add_user, Id, Nick}, State = #state{remoteusers = RemoteUsers}) ->
	{ok, Pid} = dc_client:start(State#state.host, State#state.port,  Nick, State#state.sharesize, State#state.description),
	NewUsers = RemoteUsers ++ [{Id, Nick, Pid}],
	{noreply, State#state{remoteusers = NewUsers}};

handle_cast({remove_user, Id}, State = #state{remoteusers = RemoteUsers}) ->
	{Id, _Nick, Pid} = lists:keyfind(Id, 1, RemoteUsers),
	unlink(Pid),
	exit(Pid, shutdown),
	NewUsers = lists:keydelete(Id, 1, RemoteUsers),
	{noreply, State#state{remoteusers = NewUsers}};

handle_cast({rename_user, Id, Nick}, State = #state{remoteusers = RemoteUsers}) ->
	{Id, _Nick, OldPid} = lists:keyfind(Id, 1, RemoteUsers),
	unlink(OldPid),
	exit(OldPid, shutdown),
	{ok, NewPid} = dc_client:start(State#state.host, State#state.port,  Nick, State#state.sharesize, State#state.description),
	NewUsers = lists:keyreplace(Id, 1, RemoteUsers, {Id, Nick, NewPid}),
	{noreply, State#state{remoteusers = NewUsers}};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.
