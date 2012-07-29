-module(dc_client).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

-record(state, {socket,
				nick,
				parent,
				host,
				port,
				loggedin}).

roll_xor(List) ->
	roll_xor(lists:reverse(List), []).

roll_xor([H1, H2 | Tail], Result) ->
	roll_xor([H2 | Tail], Result ++ [H1 bxor H2]);
roll_xor([_H1], Result) ->
	lists:reverse(Result).

decode_lock(LockEscaped) ->
	Lock = dcn_unescape(LockEscaped),
	First = lists:foldl(fun (X, Acc) ->
			Acc bxor X
		end, 0, [hd(Lock), 5] ++ lists:sublist(Lock, length(Lock)-1, 2)),
	dcn_escape(lists:map(fun (X) ->
			((X bsl 4) band 16#f0) bor ((X bsr 4) band 16#f) % swap higher and lower halfs of byte
		end, [First | roll_xor(Lock)])).

dcn_escape([0] ++ Tail, Result) ->
	dcn_escape(Tail, Result ++ "/%DCN000%/");
dcn_escape([5] ++ Tail, Result) ->
	dcn_escape(Tail, Result ++ "/%DCN005%/");
dcn_escape([36] ++ Tail, Result) ->
	dcn_escape(Tail, Result ++ "/%DCN036%/");
dcn_escape([96] ++ Tail, Result) ->
	dcn_escape(Tail, Result ++ "/%DCN096%/");
dcn_escape([124] ++ Tail, Result) ->
	dcn_escape(Tail, Result ++ "/%DCN124%/");
dcn_escape([126] ++ Tail, Result) ->
	dcn_escape(Tail, Result ++ "/%DCN126%/");
dcn_escape([H | Tail], Result) ->
	dcn_escape(Tail, Result ++ [H]);
dcn_escape([], Result) ->
	Result.

dcn_escape(Str) ->
	dcn_escape(Str, []).

dcn_unescape("/%DCN000%/" ++ Tail, Result) ->
	dcn_unescape(Tail, Result ++ [0]);
dcn_unescape("/%DCN005%/" ++ Tail, Result) ->
	dcn_unescape(Tail, Result ++ [5]);
dcn_unescape("/%DCN036%/" ++ Tail, Result) ->
	dcn_unescape(Tail, Result ++ [36]);
dcn_unescape("/%DCN096%/" ++ Tail, Result) ->
	dcn_unescape(Tail, Result ++ [96]);
dcn_unescape("/%DCN124%/" ++ Tail, Result) ->
	dcn_unescape(Tail, Result ++ [124]);
dcn_unescape("/%DCN126%/" ++ Tail, Result) ->
	dcn_unescape(Tail, Result ++ [126]);
dcn_unescape([H | Tail], Result) ->
	dcn_unescape(Tail, Result ++ [H]);
dcn_unescape([], Result) ->
	Result.

dcn_unescape(Str) ->
	dcn_unescape(Str, []).

send(Socket, Line) ->
	dc_socket:send_cmd(Socket, Line).

init({Parent, Host, Port, Nick}) ->
	{ok, Socket} = dc_socket:start(Host, Port),
	{ok, #state{socket = Socket,
				parent = Parent,
				nick = Nick,
				host = Host,
				port = Port,
				loggedin = false}}.

restart_connection(State) ->
	#state{socket = Socket, host = Host, port = Port} = State,
	unlink(Socket),
	exit(Socket, shutdown),
	NewSocket = dc_socket:start(Host, Port),
	State#state{socket = NewSocket}.

new_nick(Nick) ->
	Nick ++ "_".

send_join(BinNick, #state{parent = Parent, nick = OurNick}) ->
	case binary_to_list(BinNick) of
	OurNick ->
		ok;
	Nick ->
		dc:join(Parent, Nick, [])
	end.

handle_info({dc, Socket, RawMessage}, State = #state{socket=Socket, nick=Nick, parent=Parent}) ->
	case binary:split(RawMessage, <<" ">>) of
	[RawMessage] ->
		Cmd = RawMessage,
		Args = <<>>;
	[L1, L2] ->
		Cmd = L1,
		Args = L2
	end,
	io:format("Cmd: ~p\tArgs: ~p~n", [Cmd, Args]),
	case Cmd of
	<<"$Lock">> ->
		send(Socket, "$Key " ++ decode_lock(hd(string:tokens(binary_to_list(Args), " ")))),
		send(Socket, "$ValidateNick " ++ Nick),
		{noreply, State};
	<<"$ValidateDenide">> ->
		NewState = restart_connection(State#state{nick = new_nick(Nick)}),
		dc:nick_change(Parent, Nick, NewState#state.nick),
		{noreply, NewState};
	<<"$GetPass">> -> % we do not support passwords
		NewState = restart_connection(State#state{nick = new_nick(Nick)}),
		dc:nick_change(Parent, Nick, NewState#state.nick),
		{noreply, NewState};
	<<"$HubName">> ->
		{noreply, State};
	<<"$Hello">> ->
		case State#state.loggedin of
		false ->
			case NewNick = binary_to_list(Args) of
			Nick ->
				ok;
			_ ->
				dc:nick_change(Parent, Nick, NewNick)
			end,
			send(Socket, <<"$Version 1,0091">>),
			send(Socket, <<"$GetNickList">>),
			send(Socket, "$MyINFO $ALL " ++ [Args] ++ " interest$ $56Kbps" ++ [1] ++ "$no.email$0$"),
			{noreply, State#state{nick = NewNick, loggedin = true}};
		true ->
			send_join(Args, State),
			{noreply, State}
		end;
	<<"$NickList">> ->
		lists:foreach(fun(X) ->
				send_join(X, State)
			end, binary:split(Args, <<"$$">>, [global, trim])),
		{noreply, State};
	<<"$Quit">> ->
		dc:part(Parent, binary_to_list(Args), []),
		{noreply, State};
	<<"$To:">> ->
		[Header, Message] = binary:split(Args, <<"$">>),
		Sender = binary_to_list(lists:nth(3, binary:split(Header, <<" ">>, [global]))),
		Text = binary_to_list(lists:nth(2, binary:split(Message, <<" ">>))),
		dc:private_message(Parent, Sender, Nick, Text),
		{noreply, State};
	<<>> ->
		{noreply, State};
	_ ->
		case ((binary:first(Cmd) == $<) and (binary:last(Cmd) == $>)) of
		true ->
			Sender = binary_to_list(binary:part(Cmd, 1, size(Cmd) - 2)),
			dc:message(Parent, Sender, binary_to_list(Args)),
			{noreply, State};
		false ->
			case State#state.loggedin of
			false ->
				error_logger:error_msg("Hub sent unknown command before completing registration: ~p~n", [RawMessage]),
				NewState = restart_connection(State#state{nick = new_nick(Nick)}),
				dc:nick_change(Parent, Nick, NewState#state.nick),
				{noreply, NewState};
			true ->
				{noreply, State}
			end
		end
	end.

terminate(_, _) ->
	ok.

code_change(_,State,_) -> {ok, State}.
handle_call(_,_,State) -> {noreply, State}.
handle_cast(_,State) -> {noreply, State}.
