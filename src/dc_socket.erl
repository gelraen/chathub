-module(dc_socket).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2, send_cmd/2]).

-record(state, {parent, buffer = [], socket}).

start(Host, Port) ->
	gen_server:start_link(dc_socket, {self(), Host, Port}, []).

str_replace(String, Old, New) ->
	str_replace(String, Old, New, []).

str_replace([], _Old, _New, Result) ->
	Result;
str_replace(String, Old, New, Result) ->
	case lists:prefix(Old, String) of
	true ->
		str_replace(lists:nthtail(length(Old), String), Old, New, Result ++ New);
	false ->
		str_replace(tl(String), Old, New, Result ++ [hd(String)])
	end.

send_cmd(Pid, Cmd) ->
	gen_server:cast(Pid, {write, Cmd}).

init({Parent, Host, Port}) ->
	case gen_tcp:connect(Host, Port, [list, {active, true}, {packet, raw}]) of
	{ok, Socket} ->
		{ok, #state{socket = Socket, parent = Parent}};
	{error, Reason} ->
		{stop, Reason}
	end.

handle_info({tcp, Socket, RawData}, State = #state{socket = Socket, buffer = Buffer, parent = Pid}) ->
	NewState = case string:chr(RawData, $|) of
	0 ->
		State#state{buffer = Buffer ++ RawData};
	N when N > 0 ->
		Tokens = string:tokens(Buffer ++ RawData, "|"),
		case lists:last(RawData) of
		$| ->
			Commands = Tokens,
			Rest = [];
		_ ->
			Commands = lists:sublist(Tokens, length(Tokens) - 1),
			Rest = lists:last(Tokens)
		end,
		lists:foreach(fun(Cmd) ->
			Pid ! {dc, self(), str_replace(Cmd, "&#124;", "|")}
			end, Commands),
		State#state{buffer = Rest}
	end,
	{noreply, NewState}.

terminate(_, #state{socket=Socket}) ->
	gen_tcp:close(Socket).

code_change(_,State,_) -> {ok, State}.
handle_call(_,_,State) -> {noreply, State}.

handle_cast({write, Data}, State = #state{socket=Socket}) ->
	gen_tcp:send(Socket, str_replace(Data, "|", "&#124;") ++ "|"),
	{noreply, State};
handle_cast(_,State) -> {noreply, State}.
