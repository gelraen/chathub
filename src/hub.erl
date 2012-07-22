-module(hub).
-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start/1]).

-export([send_msg/2, add_user/2, remove_user/2, rename_user/3]).

-include("hub.hrl").

-record(state, {name,
		children = [],
		config = [],
		users = []}).

start({hub, Name, _, _} = Config) ->
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

send_msg(Pid, Msg) ->
    gen_server:cast(Pid, {msg, self(), Msg}).

% returns {ok, UserId}
add_user(Pid, Name) ->
	gen_server:call(Pid, {add_user, Name}).

remove_user(Pid, Id) ->
	gen_server:call(Pid, {remove_user, Id}).

rename_user(Pid, Id, NewName) ->
	gen_server:call(Pid, {rename_user, Id, NewName}).

init({hub, Name, Config, ChildConfig}) ->
	process_flag(trap_exit, true),
    Children = lists:map(fun(X) ->
				spawn_child(X, Name)
			end,
		    ChildConfig),
    {ok, #state{name = Name, children = Children, config = Config}}.

spawn_child({Type, Args} = Config, ParentName) ->
	{ok, Pid} = Type:start(Args, ParentName),
	{Pid, Config}.

call_for_all_except_sender(SenderPid, Func, Args, State) ->
	lists:foreach(fun ({Pid, _}) when Pid == SenderPid ->
				ok;
			({Pid, {Type, _}}) ->
				apply(Type, Func, [Pid] ++ Args)
			end, State#state.children),
	ok.

gen_user_id(Name, Pid, State) ->
	Id = {random:uniform(16#fffffff), Pid},
	case lists:member(Id, State#state.users) of
	true ->
		gen_user_id(Name, Pid, State);
	false ->
		Id
	end.

pid_from_id({_, Pid}, _State) ->
	Pid.

handle_call({add_user, Name}, {Pid, _}, State) ->
	Id = gen_user_id(Name, Pid, State),
	NewUsers = State#state.users ++ [{Id, Name}],
	call_for_all_except_sender(Pid, user_added, [Id, Name], State),
	{reply, {ok, Id}, State#state{users=NewUsers}};

handle_call({remove_user, Id}, {Pid, _}, State) ->
	NewUsers = case lists:keymember(Id, 1, State#state.users) of
	true ->
		call_for_all_except_sender(Pid, user_removed, [Id], State),
		lists:keydelete(Id, 1, State#state.users);
	false ->
		State#state.users
	end,
	{reply, ok, State#state{users=NewUsers}};

handle_call({rename_user, Id, NewName}, {Pid, _}, State) ->
	NewUsers = case lists:keymember(Id, 1, State#state.users) of
	true ->
		call_for_all_except_sender(Pid, user_renamed, [Id, NewName], State),
		lists:keyreplace(Id, 1, State#state.users, {Id, NewName});
	false ->
		State#state.users
	end,
	{reply, ok, State#state{users=NewUsers}};

handle_call(_, _From, State) ->
    {reply, ignored, State}.

handle_cast({msg, From, Msg}, State) ->
	call_for_all_except_sender(From, send_msg, [Msg], State),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
	error_logger:warning_msg("Proto handler ~p died with reason: ~p~nState: ~p~n", [Pid, Reason, State]),
	% remove users from that proto_handler
	{Removed, NewUsers} = lists:partition(fun ({Id, _Name}) ->
			pid_from_id(Id, State) == Pid
		end, State#state.users),
	lists:foreach(fun({Id, _Name}) ->
			call_for_all_except_sender(Pid, user_removed, [Id], State)
		end, Removed),
	% restart handler
	{Pid, ChildConfig} = lists:keyfind(Pid, 1, State#state.children),
	{NewPid, ChildConfig} = spawn_child(ChildConfig, State#state.name),
	% send all current users to spawned handler
	{Type, _} = ChildConfig,
	lists:foreach(fun ({Id, Name}) ->
			Type:user_added(NewPid, Id, Name)
		end, NewUsers),
	NewChildren = lists:keyreplace(Pid, 1, State#state.children, {NewPid, ChildConfig}),
	{noreply, State#state{users=NewUsers, children=NewChildren}};

handle_info(Data, State) ->
	error_logger:info_msg("Hub \"~p\" received message: ~p~n", [State#state.name, Data]),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, State) ->
	error_logger:info_msg("Hub \"~p\" terminates with reason: ~p~n", [State#state.name, Reason]),
    ok.

