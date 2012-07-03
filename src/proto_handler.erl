-module(proto_handler).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start,2}, % Config, Parent
    {send_msg,2} % Pid, Msg
];
behaviour_info(_) ->
    undefined.
