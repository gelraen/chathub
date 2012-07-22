-module(irc_client).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

-record(state, {socket,
				nick,
				parent,
				channel,
				prefix_chars = "@+"}).

-record(cmd, {name, prefix, args}).

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

init({Parent, Host, Port, Channel, Nick, RealName, UserName}) ->
	case gen_tcp:connect(Host, Port, [list, 
			{active, true},
			{packet, line},
			{packet_size, 512}]) of
	{ok, Socket} ->
		send(Socket, "NICK " ++ Nick),
		send(Socket, "USER " ++ UserName ++ " 0 0 :" ++ RealName),
		{ok, #state{socket=Socket, nick=Nick, channel=Channel, parent=Parent}};
	{error, Reason} ->
		{stop, Reason}
	end.

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
				irc:join(State#state.parent, Nick, [])
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
					irc:join(State#state.parent, StrippedNick, [])
				end,
				string:tokens(lists:nth(4, Command#cmd.args), " "));
		_ ->
			ok
		end,
		{noreply, State};
	"PRIVMSG" ->
		case hd(Command#cmd.args) of
		Channel ->
			irc:message(State#state.parent, nick_from_prefix(Command#cmd.prefix), lists:nth(2, Command#cmd.args));
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
			irc:message(State#state.parent, nick_from_prefix(Command#cmd.prefix), lists:nth(2, Command#cmd.args));
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
		irc:nick_change(State#state.parent, OldNick, NewNick),
		{noreply, State};
	"PART" ->
		case hd(Command#cmd.args) of
		Channel ->
			irc:part(State#state.parent, nick_from_prefix(Command#cmd.prefix), tl(Command#cmd.args));
		_ ->
			ok
		end,
		{noreply, State};
	"QUIT" ->
		irc:part(State#state.parent, nick_from_prefix(Command#cmd.prefix), tl(Command#cmd.args)),
		{noreply, State};
	_ ->
		{noreply, State}
	end.

terminate(_, #state{socket=Socket}) ->
	send(Socket, "QUIT :Process terminated"),
	gen_tcp:close(Socket).

code_change(_,State,_) -> {ok, State}.
handle_call(_,_,State) -> {noreply, State}.
handle_cast(_,State) -> {noreply, State}.
