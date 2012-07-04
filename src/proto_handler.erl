-module(proto_handler).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{start,2}, % Config, Parent
	{send_msg,2}, % Pid, Msg
	{user_added,3}, % Pid, UserId, Name
	{user_removed,2}, % Pid, UserId
	{user_renamed,3} % Pid, UserId, NewName
];
behaviour_info(_) ->
	undefined.
