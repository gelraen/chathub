-module(dc_socket).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2, send_cmd/2]).

-record(state, {parent, buffer = <<>>, socket}).

start(Host, Port) ->
	gen_server:start_link(dc_socket, {self(), Host, Port}, []).

send_cmd(Pid, Cmd) ->
	gen_server:cast(Pid, {write, Cmd}).

init({Parent, Host, Port}) ->
	case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]) of
	{ok, Socket} ->
		{ok, #state{socket = Socket, parent = Parent}};
	{error, Reason} ->
		{stop, Reason}
	end.

handle_info({tcp, Socket, RawData}, State = #state{socket = Socket, buffer = Buffer, parent = Pid}) ->
	Tokens = binary:split(<<Buffer/binary, RawData/binary>>, <<"|">>, [global]),
	Commands = lists:sublist(Tokens, length(Tokens) - 1),
	Rest = lists:last(Tokens),
	lists:foreach(fun(Cmd) ->
			Pid ! {dc, self(), binary:replace(Cmd, <<"&#124;">>, <<"|">>)}
		end, Commands),
	{noreply, State#state{buffer = Rest}}.

terminate(_, #state{socket=Socket}) ->
	gen_tcp:close(Socket).

code_change(_,State,_) -> {ok, State}.
handle_call(_,_,State) -> {noreply, State}.

handle_cast({write, Data}, State = #state{socket=Socket}) ->
	gen_tcp:send(Socket, [binary:replace(iolist_to_binary(Data), <<"|">>, <<"&#124;">>), <<"|">>]),
	{noreply, State};
handle_cast(_,State) -> {noreply, State}.
