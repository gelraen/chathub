-module(irc_client).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([send_msg/2, send_privmsg/3, change_nick/2, start/6, start_master/6]).

-record(state, {socket,
				nick,
				parent,
				channel,
				prefix_chars = "@+",
				master = false}).

-record(cmd, {name, prefix, args}).

send_msg(Pid, Text) ->
	gen_server:cast(Pid, {send_msg, Text}).

send_privmsg(Pid, To, Text) ->
	gen_server:cast(Pid, {send_privmsg, To, Text}).

change_nick(Pid, NewNick) ->
	gen_server:cast(Pid, {change_nick, NewNick}).

start(Host, Port, Channel, Nick, RealName, UserName) ->
	gen_server:start_link(?MODULE, {self(), Host, Port, Channel, Nick, RealName, UserName, false}, []).

start_master(Host, Port, Channel, Nick, RealName, UserName) ->
	gen_server:start_link(?MODULE, {self(), Host, Port, Channel, Nick, RealName, UserName, true}, []).

first_token(Tok, []) ->
	{Tok, []};
first_token(Tok, [H|Message]) ->
	case H of
	$  ->
		{Tok, Message};
	_ ->
		first_token(Tok ++ [H], Message)
	end.

split_message(Tokens, []) ->
	Tokens;
split_message(Tokens, Message) ->
	case hd(Message) of
	$: ->
		Tokens ++ [tl(Message)];
	_ ->
		{Token, Tail} = first_token([], Message),
		split_message(Tokens ++ [Token], Tail)
	end.
split_message(Message) ->
	case hd(Message) of
	$: ->
		{Prefix, Tail} = first_token([], tl(Message)),
		split_message([Prefix], Tail);
	_ ->
		split_message([""], Message)
	end.

parse_message(Message) ->
	[Prefix,Name|Args] = split_message(string:strip(string:strip(Message, right, $\n), right, $\r)),
	Result = #cmd{name=Name, prefix=Prefix, args=Args},
	Result.

nick_from_prefix(Prefix) ->
	string:sub_string(Prefix, 1, string:chr(Prefix, $!) - 1).

send(Socket, Line) ->
	gen_tcp:send(Socket, Line ++ "\r\n").

replace_unsafe_chars([], Result) ->
	Result;
replace_unsafe_chars([H | Tail], Result) when (H >= $A) andalso (H =< $Z);
		(H >= $a) andalso (H =< $z);
		(H >= $0) andalso (H =< $9);
		H == $[;
		H == $];
		H == ${;
		H == $};
		H == $\\;
		H == $`;
		H == $_;
		H == $^;
		H == $| ->
	replace_unsafe_chars(Tail, Result ++ [H]);
replace_unsafe_chars([_ | Tail], Result) ->
	replace_unsafe_chars(Tail, Result ++ "^").

replace_unsafe_chars(Nick) ->
	replace_unsafe_chars(Nick, "^").

init({Parent, Host, Port, Channel, Nick, RealName, UserName, Master}) ->
	case gen_tcp:connect(Host, Port, [list, 
			{active, true},
			{packet, line},
			{packet_size, 512}]) of
	{ok, Socket} ->
		send(Socket, "NICK " ++ Nick),
		send(Socket, "USER " ++ UserName ++ " 0 0 :" ++ RealName),
		{ok, #state{socket=Socket, nick=Nick, channel=Channel, parent=Parent, master = Master}};
	{error, Reason} ->
		{stop, Reason}
	end.

send_join(_, #state{master = false}) -> ok;
send_join(Nick, #state{parent = Parent, master = true}) ->
	irc:join(Parent, Nick, []).

send_part(_, _, #state{master = false}) -> ok;
send_part(Nick, Data, #state{parent = Parent, master = true}) ->
	irc:part(Parent, Nick, Data).

send_message(_ ,_ ,#state{master = false}) -> ok;
send_message(Nick, Text, #state{parent = Parent, master = true}) ->
	irc:message(Parent, Nick, Text).

send_nick_change(_ ,_ ,#state{master = false}) -> ok;
send_nick_change(Old, New, #state{parent = Parent, master = true}) ->
	irc:nick_change(Parent, Old, New).

handle_info({tcp, Socket, RawMessage}, State = #state{socket=Socket}) ->
	Command = parse_message(RawMessage),
	Channel = State#state.channel,
	OurNick = State#state.nick,
	case Command#cmd.name of
	"PING" ->
		send(Socket, "PONG :" ++ hd(Command#cmd.args)),
		{noreply, State};
	"001" ->
		send(Socket, "JOIN " ++ State#state.channel),
		{noreply, State};
	"005" -> % ISUPPORT
		NewState = lists:foldl(fun ("PREFIX=" ++ Value, State) ->
				NewPrefixChars = case Value of
				"" ->
					"";
				_ ->
					tl(lists:dropwhile(fun(X) -> X =/= $) end, Value))
				end,
				State#state{prefix_chars=NewPrefixChars};
			(_, State) ->
				State
			end, State, tl(Command#cmd.args)),
		{noreply, NewState};
	"432" -> % Incorrect nickname
		NewNick = replace_unsafe_chars(OurNick),
		error_logger:info_msg("irc_client: replacing nick \"~s\" with \"~s\"~n", [OurNick, NewNick]),
		send(Socket, "NICK " ++ NewNick),
		irc:nick_change(State#state.parent, OurNick, NewNick),
		{noreply, State#state{nick = NewNick}};
	"433" -> % Nickname in use
		NewNick = OurNick ++ "_",
		send(Socket, "NICK " ++ NewNick),
		irc:nick_change(State#state.parent, OurNick, NewNick),
		{noreply, State#state{nick=NewNick}};
	"ERROR" ->
		error_logger:error_msg("Something bad happened: ~p~nState:~p~n", [Command#cmd.args, State]),
		{stop, {irc_error, Command}, State};
	"JOIN" ->
		case hd(Command#cmd.args) of
		Channel ->
			case nick_from_prefix(Command#cmd.prefix) of
			OurNick ->
				ok;
			Nick ->
				send_join(Nick, State)
			end;
		_ ->
			ok
		end,
		{noreply, State};
	"353" -> % NAMES list
		case lists:nth(3, Command#cmd.args) of
		Channel ->
			lists:foreach(fun (Nick) ->
					StrippedNick = case lists:member(hd(Nick), State#state.prefix_chars) of
					true ->
						tl(Nick);
					false ->
						Nick
					end,
					case StrippedNick of
					OurNick ->
						ok;
					_ ->
						send_join(StrippedNick, State)
					end
				end,
				string:tokens(lists:nth(4, Command#cmd.args), " "));
		_ ->
			ok
		end,
		{noreply, State};
	"PRIVMSG" ->
		case hd(Command#cmd.args) of
		Channel ->
			send_message(nick_from_prefix(Command#cmd.prefix), lists:nth(2, Command#cmd.args), State);
		OurNick ->
			irc:private_message(State#state.parent,
								nick_from_prefix(Command#cmd.prefix),
								hd(Command#cmd.args),
								lists:nth(2, Command#cmd.args));
		_ ->
			ok
		end,
		{noreply, State};
	"NOTICE" ->
		case hd(Command#cmd.args) of
		Channel ->
			send_message(nick_from_prefix(Command#cmd.prefix), lists:nth(2, Command#cmd.args), State);
		OurNick ->
			irc:private_message(State#state.parent,
								nick_from_prefix(Command#cmd.prefix),
								hd(Command#cmd.args),
								lists:nth(2, Command#cmd.args));
		_ ->
			ok
		end,
		{noreply, State};
	"NICK" ->
		OldNick = nick_from_prefix(Command#cmd.prefix),
		NewNick = hd(Command#cmd.args),
		case OldNick of
		OurNick ->
			irc:nick_change(State#state.parent, OurNick, NewNick),
			{noreply, State#state{nick = NewNick}};
		_ ->
			send_nick_change(OldNick, NewNick, State),
			{noreply, State}
		end;
	"PART" ->
		case hd(Command#cmd.args) of
		Channel ->
			send_part(nick_from_prefix(Command#cmd.prefix), tl(Command#cmd.args), State);
		_ ->
			ok
		end,
		{noreply, State};
	"QUIT" ->
		send_part(nick_from_prefix(Command#cmd.prefix), tl(Command#cmd.args), State),
		{noreply, State};
	_ ->
		{noreply, State}
	end.

terminate(_, #state{socket=Socket}) ->
	send(Socket, "QUIT :Process terminated"),
	gen_tcp:close(Socket).

code_change(_,State,_) -> {ok, State}.
handle_call(_,_,State) -> {noreply, State}.

handle_cast({send_msg, Text}, State = #state{socket = Socket, channel = Channel}) ->
	send(Socket, "PRIVMSG " ++ Channel ++ " :" ++ Text),
	{noreply, State};

handle_cast({send_privmsg, To, Text}, State = #state{socket = Socket}) ->
	send(Socket, "PRIVMSG " ++ To ++ " :" ++ Text),
	{noreply, State};

handle_cast({change_nick, NewNick}, State = #state{socket = Socket}) ->
	send(Socket, "NICK " ++ NewNick),
	{noreply, State};

handle_cast(_,State) -> {noreply, State}.
