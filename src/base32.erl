-module(base32).

-export([encode/1, decode/1]).

encode(Data) when is_binary(Data) ->
	encode(Data, []);
encode(Data) when is_list(Data) ->
	encode(list_to_binary(lists:flatten(Data))).

encode_char(X) ->
	element(X+1,
	{$A, $B, $C, $D, $E, $F, $G, $H,
	 $I, $J, $K, $L, $M, $N, $O, $P,
	 $Q, $R, $S, $T, $U, $V, $W, $X,
	 $Y, $Z, $2, $3, $4, $5, $6, $7}).

encode(<<Block:5/binary, Rest/binary>>, Result) ->
	encode(Rest, Result ++ encode_block(Block));
encode(<<>>, Result) ->
	Result;
encode(Block, Result) ->
	Result ++ encode_block(Block).

encode_block(<<Block:5/binary>>) ->
	<<A:5, B:5, C:5, D:5, E:5, F:5, G:5, H:5>> = Block,
	lists:map(fun(X) -> encode_char(X) end, [A, B, C, D, E, F, G, H]);
encode_block(<<Block:4/binary>>) ->
	<<A:5, B:5, C:5, D:5, E:5, F:5, G:2>> = Block,
	lists:map(fun(X) -> encode_char(X) end, [A, B, C, D, E, F, G bsl 3]) ++ "=";
encode_block(<<Block:3/binary>>) ->
	<<A:5, B:5, C:5, D:5, E:4>> = Block,
	lists:map(fun(X) -> encode_char(X) end, [A, B, C, D, E bsl 1]) ++ "===";
encode_block(<<Block:2/binary>>) ->
	<<A:5, B:5, C:5, D:1>> = Block,
	lists:map(fun(X) -> encode_char(X) end, [A, B, C, D bsl 4]) ++ "====";
encode_block(<<Block:1/binary>>) ->
	<<A:5, B:3>> = Block,
	lists:map(fun(X) -> encode_char(X) end, [A, B bsl 2]) ++ "======".

decode(Data) ->
	iolist_to_binary(decode(Data, [])).

decode_char(Char) ->
	string:chr("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", Char) - 1.

decode([A,B,C,D,E,F,G,H|Rest], Result) ->
	decode(Rest, Result ++ [decode_block([A,B,C,D,E,F,G,H])]);
decode(Chars, Result) when length(Chars) == 7 ->
	Result ++ [decode_block(Chars ++ "=")];
decode(Chars, Result) when length(Chars) == 5 ->
	Result ++ [decode_block(Chars ++ "===")];
decode(Chars, Result) when length(Chars) == 4 ->
	Result ++ [decode_block(Chars ++ "====")];
decode(Chars, Result) when length(Chars) == 2 ->
	Result ++ [decode_block(Chars ++ "======")];
decode([], Result) ->
	Result.

decode_block([X1, X2, $=, $=, $=, $=, $=, $=]) ->
	[A, B] = lists:map(fun(X) -> decode_char(X) end, [X1, X2]),
	<<A:5, (B bsr 2):3>>;
decode_block([X1, X2, X3, X4, $=, $=, $=, $=]) ->
	[A, B, C, D] = lists:map(fun(X) -> decode_char(X) end, [X1, X2, X3, X4]),
	<<A:5, B:5, C:5, (D bsr 4):1>>;
decode_block([X1, X2, X3, X4, X5, $=, $=, $=]) ->
	[A, B, C, D, E] = lists:map(fun(X) -> decode_char(X) end, [X1, X2, X3, X4, X5]),
	<<A:5, B:5, C:5, D:5, (E bsr 1):4>>;
decode_block([X1, X2, X3, X4, X5, X6, X7, $=]) ->
	[A, B, C, D, E, F, G] = lists:map(fun(X) -> decode_char(X) end, [X1, X2, X3, X4, X5, X6, X7]),
	<<A:5, B:5, C:5, D:5, E:5, F:5, (G bsr 3):2>>;
decode_block(Chars) ->
	[A, B, C, D, E, F, G, H] = lists:map(fun(X) -> decode_char(X) end, Chars),
	<<A:5, B:5, C:5, D:5, E:5, F:5, G:5, H:5>>.
