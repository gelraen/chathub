-module(adc_client).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([protocol/2, identify/2, verify/2, normal/2, fail/2]).

-record(state, {socket,
				host,
				port,
				parent,
				nick,
				share,
				descr,
				sid,
				pid,
				cid,
				master = false}).

-record(cmd, {type = 0, name = <<>>, from_sid = <<>>, to_sid = <<>>, args = [], features = <<>>}).

start(Host, Port, Nick, Share, Descr) ->
	gen_fsm:start_link(?MODULE, {self(), Host, Port, Nick, Share, Descr, false}, []).

start_master(Host, Port, Nick, Share, Descr) ->
	gen_fsm:start_link(?MODULE, {self(), Host, Port, Nick, Share, Descr, true}, []).


init({Parent, Host, Port, Nick, Share, Descr, Master}) ->
	case gen_tcp:connect(Host, Port, [binary, 
			{active, true},
			{packet, line},
			{packet_size, 0}]) of
	{ok, Socket} ->
		gen_fsm:send_event(self(), init),
		{ok, protocol, #state{socket=Socket, nick=Nick, parent=Parent, master = Master,
			host = Host, port = Port, share = Share, descr = Descr}};
	{error, Reason} ->
		{stop, Reason}
	end.

handle_info({tcp, Socket, RawMessage}, State, StateData = #state{socket=Socket}) ->
	gen_fsm:send_event(self(), parse_msg(RawMessage)),
	{next_state, State, StateData}.

handle_event(_, State, StateData) ->
	{next_state, State, StateData}.

handle_sync_event(_, _, State, StateData) ->
	{next_state, State, StateData}.

terminate(_, _, #state{socket=Socket}) ->
	gen_tcp:close(Socket).

protocol(init, #state{socket = Socket} = StateData) ->
	PID = crypto:rand_bytes(192 div 8),
	{A, B, C} = tiger:hash(PID),
	CID = <<A:64/little, B:64/little, C:64/little>>,
	send_cmd(Socket, #cmd{type = $H, name = <<"SUP">>, args = [<<"ADBASE">>, <<"ADTIGR">>]}),
	{next_state, protocol, StateData#state{pid = PID, cid = CID}};

protocol(#cmd{name = <<"SID">>, args = [OurSID]}, StateData) ->
	identify(init, StateData#state{sid = OurSID});

protocol(#cmd{name = <<"STA">>, args = [Code | _Tail]}, StateData) ->
	case binary:first(Code) of
	$0 ->
		{next_state, protocol, StateData};
	_ ->
		fail(restart, StateData)
	end;

protocol(_, StateData) ->
	{next_state, protocol, StateData}.

identify(init, #state{socket = Socket, share = Share, nick = Nick, descr = Descr, pid = PID, cid = CID, sid = SID} = StateData) ->
	ShareSize = list_to_binary(integer_to_list(Share)),
	EncodedNick = list_to_binary(Nick),
	EncodedDescr = list_to_binary(Descr),
	EncodedPID = list_to_binary(string:strip(base32:encode(PID), right, $=)),
	EncodedCID = list_to_binary(string:strip(base32:encode(CID), right, $=)),
	send_cmd(Socket, #cmd{type = $B, name = <<"INF">>, from_sid = SID,
				args = [<<"SS", ShareSize/binary>>,
					<<"VE", "chathub">>,
					<<"NI", EncodedNick/binary>>,
					<<"DE", EncodedDescr/binary>>,
					<<"PD", EncodedPID/binary>>,
					<<"ID", EncodedCID/binary>>]}),
	{next_state, identify, StateData};

identify(#cmd{name = <<"GPA">>} = Cmd, StateData) ->
	verify(Cmd, StateData);

identify(#cmd{type = $B, name = <<"INF">>} = Cmd, StateData) ->
	normal(Cmd, StateData);

identify(#cmd{name = <<"QUI">>, args = [SID | Args]}, #state{sid = SID} = StateData) ->
	fail({stop, {"QUI command from hub", Args}}, StateData);

identify(#cmd{name = <<"STA">>, args = [Code | _Tail]}, StateData) ->
	case binary:first(Code) of
	$0 ->
		{next_state, identify, StateData};
	_ ->
		fail(restart, StateData)
	end;

identify(_, StateData) ->
	{next_state, identify, StateData}.

verify(#cmd{name = <<"QUI">>, args = [SID | Args]}, #state{sid = SID} = StateData) ->
	{stop, {"QUI command from hub", Args}, StateData};

verify(#cmd{name = <<"GPA">>}, StateData) ->
	fail(restart, StateData);

verify(#cmd{name = <<"STA">>, args = [Code | _Tail]}, StateData) ->
	case binary:first(Code) of
	$0 ->
		{next_state, verify, StateData};
	_ ->
		fail(restart, StateData)
	end;

verify(_, StateData) ->
	{next_state, verify, StateData}.

fail({stop, Reason}, StateData) ->
	{stop, Reason, StateData};

fail(restart, #state{socket = Socket, host = Host, port = Port, nick = Nick} = StateData) ->
	unlink(Socket),
	exit(Socket, shutdown),
	{ok, NewSocket} = gen_tcp:connect(Host, Port, [binary,
			{active, true},
			{packet, line},
			{packet_size, 0}]),
	NewNick = Nick ++ "_",
	protocol(init, StateData#state{socket = NewSocket, nick = NewNick}).

normal(_Cmd, StateData) ->
	{next_state, normal, StateData}.

code_change(_, State, StateData ,_) -> {ok, State, StateData}.



unescape(Data) -> list_to_binary(unescape(binary_to_list(Data), [])).

unescape("\\\\" ++ Rest, Result) -> unescape(Rest, Result ++ "\\");
unescape("\\n" ++ Rest, Result) -> unescape(Rest, Result ++ "\n");
unescape("\\s" ++ Rest, Result) -> unescape(Rest, Result ++ " ");
unescape([H | Rest], Result) -> unescape(Rest, Result ++ [H]);
unescape([], Result) -> Result.

escape(Data) ->
	binary:replace(binary:replace(binary:replace(Data, <<"\\">>, <<"\\\\">>), <<" ">>, <<"\\s">>), <<"\n">>, <<"\\n">>).

parse_msg(RawData) ->
	Tokens = binary:split(case binary:last(RawData) of
	$\n ->
		binary:part(RawData, 0, size(RawData) - 1);
	_ ->
		RawData
	end, <<" ">>, [global]),
	{Cmd, Rest} = case Tokens of
	[<<Type:8, Name/binary>> | EscapedRest] ->
		{#cmd{type = Type, name = Name}, lists:map(fun (X) -> unescape(X) end, EscapedRest)};
	_ ->
		{#cmd{}, []}
	end,
	case Cmd#cmd.type of
	$B ->
		case Rest of
		[From | Args] ->
			Cmd#cmd{from_sid = From, args = Args};
		_ ->
			#cmd{}
		end;
	$C -> % should not appear in our use case
		Cmd#cmd{args = Rest};
	$I ->
		Cmd#cmd{args = Rest};
	$H ->
		Cmd#cmd{args = Rest};
	$D ->
		case Rest of
		[From, To | Args] ->
			Cmd#cmd{from_sid = From, to_sid = To, args = Args};
		_ ->
			#cmd{}
		end;
	$E ->
		case Rest of
		[From, To | Args] ->
			Cmd#cmd{from_sid = From, to_sid = To, args = Args};
		_ ->
			#cmd{}
		end;
	$F ->
		case Rest of
		[From, Features | Args] ->
			Cmd#cmd{from_sid = From, args = Args, features = Features};
		_ ->
			#cmd{}
		end;
	$U -> % should not appear in our use case
		case Rest of
		[From | Args] ->
			Cmd#cmd{from_sid = From, args = Args}; % XXX: From is CID, not SID
		_ ->
			#cmd{}
		end;
	_ ->
		#cmd{}
	end.

send_cmd(Socket, #cmd{type = Type, args = Args, name = Name, from_sid = From, to_sid = To, features = Features}) ->
	EscapedArgs = lists:map(fun(X) ->
			[$ , escape(X)]
		end, Args),
	Data = case Type of
	$B ->
		[Type, Name, $ , From] ++ EscapedArgs;
	$C ->
		[Type, Name] ++ EscapedArgs;
	$I ->
		[Type, Name] ++ EscapedArgs;
	$H ->
		[Type, Name] ++ EscapedArgs;
	$D ->
		[Type, Name, $ , From, $ , To] ++ EscapedArgs;
	$E ->
		[Type, Name, $ , From, $ , To] ++ EscapedArgs;
	$F ->
		[Type, Name, $ , From, $ , Features] ++ EscapedArgs;
	$U ->
		[Type, Name, $ , From] ++ EscapedArgs;
	_ ->
		[]
	end,
	gen_tcp:send(Socket, [Data, $\n]).
