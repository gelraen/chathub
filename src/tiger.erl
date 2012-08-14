-module(tiger).

-export([hash/1, hash_string/1]).

-include("tiger_sboxes.hrl"). % generated definitions for t1/1, t2/1, t3/1 and t4/1

-define(NOT(Val), ((16#FFFFFFFFFFFFFFFF bxor (Val)) band 16#FFFFFFFFFFFFFFFF)).
-define(SHL(Val, Bits), (((Val) bsl (Bits)) band 16#FFFFFFFFFFFFFFFF)).
-define(SHR(Val, Bits), (((Val) bsr (Bits)) band 16#FFFFFFFFFFFFFFFF)).
-define(ADD(A, B), (((A) + (B)) band 16#FFFFFFFFFFFFFFFF)).
-define(SUB(A, B), (((A) - (B)) band 16#FFFFFFFFFFFFFFFF)).
-define(MUL(A, B), (((A) * (B)) band 16#FFFFFFFFFFFFFFFF)).
-define(XOR(A, B), (((A) bxor (B)) band 16#FFFFFFFFFFFFFFFF)).

hash_string(Data) ->
	{A, B, C} = hash(Data),
	lists:flatten(io_lib:format("~16.16.0B~16.16.0B~16.16.0B", [A, B, C])).

hash(Data) when is_binary(Data) ->
	hash(Data, {16#0123456789ABCDEF, 16#FEDCBA9876543210, 16#F096A5B4C3B2E187}, size(Data));
hash(Data) when is_list(Data) ->
	hash(list_to_binary(lists:flatten(Data))).

hash(<<Block:64/binary, Rest/binary>>, Regs, Length) ->
	NewRegs = hash_block(Block, Regs),
	hash(Rest, NewRegs, Length);

hash(Data, Regs, Length) ->
	case pad_block(Data, Length*8) of
	[Block] ->
		hash_block(Block, Regs);
	[Block0, Block1] ->
		hash_block(Block1, hash_block(Block0, Regs))
	end.

pad_block(Data, TotalLength) when (size(Data) =< (64-8-1)) ->
	Zeros = binary:copy(<<0>>, 64 - size(Data) - 8 - 1),
	[<<Data/binary, 1, Zeros/binary, TotalLength:64/little>>];

pad_block(Data, TotalLength) ->
	Zeros = binary:copy(<<0>>, 64 - size(Data) - 1),
	Block0 = <<Data/binary, 1, Zeros/binary>>,
	Block1 = <<0:448, TotalLength:64/little>>,
	[Block0, Block1].

hash_block(<<Block:64/binary>>, {A, B, C}) ->
	{A1, B1, C1} = pass(Block, 5, {A, B, C}),
	X1 = key_schedule(Block),
	{C2, A2, B2} = pass(X1, 7, {C1, A1, B1}),
	X2 = key_schedule(X1),
	{B3, C3, A3} = pass(X2, 9, {B2, C2, A2}),
	NewA = ?XOR(A3, A),
	NewB = ?SUB(B3, B),
	NewC = ?ADD(C3, C),
	{NewA, NewB, NewC}.

pass(Data, Mul, Regs) ->
	<<X0:64/little, X1:64/little, X2:64/little, X3:64/little, X4:64/little, X5:64/little, X6:64/little, X7:64/little>> = Data,
	{NewC, NewA, NewB} = lists:foldl(fun(X, {A, B, C}) ->
			{A1, B1, C1} = round(A, B, C, X, Mul),
			{B1, C1, A1}
		end, Regs, [X0,X1,X2,X3,X4,X5,X6,X7]),
	{NewA, NewB, NewC}.

round(A, B, C, X, Mul) ->
	NewC = ?XOR(C, X),
	<<C0:8, C1:8, C2:8, C3:8, C4:8, C5:8, C6:8, C7:8>> = <<NewC:64/little>>,
	NewA = ?SUB(A, ?XOR(t1(C0), ?XOR(t2(C2), ?XOR(t3(C4), t4(C6))))),
	NewB = ?MUL(Mul, ?ADD(B, ?XOR(t4(C1), ?XOR(t3(C3), ?XOR(t2(C5), t1(C7)))))),
	{NewA, NewB, NewC}.

key_schedule(<<X0:64/little, X1:64/little, X2:64/little, X3:64/little, X4:64/little, X5:64/little, X6:64/little, X7:64/little>>) ->
	{XX0, XX1, XX2, XX3, XX4, XX5, XX6, XX7} =
		key_schedule2(key_schedule1({X0,X1,X2,X3,X4,X5,X6,X7})),
	<<XX0:64/little, XX1:64/little, XX2:64/little, XX3:64/little,
		XX4:64/little, XX5:64/little, XX6:64/little, XX7:64/little>>.

key_schedule1({X0,X1,X2,X3,X4,X5,X6,X7}) ->
	XX0 = ?SUB(X0, ?XOR(X7, 16#A5A5A5A5A5A5A5A5)), % x0 -= x7 ^ 0xA5A5A5A5A5A5A5A5;
	XX1 = ?XOR(X1, XX0), % x1 ^= x0;
	XX2 = ?ADD(X2, XX1), % x2 += x1;
	XX3 = ?SUB(X3, ?XOR(XX2, ?SHL(?NOT(XX1),19))), % x3 -= x2 ^ ((~x1)<<19);
	XX4 = ?XOR(X4, XX3), % x4 ^= x3;
	XX5 = ?ADD(X5, XX4), % x5 += x4;
	XX6 = ?SUB(X6, ?XOR(XX5, ?SHR(?NOT(XX4),23))), % x6 -= x5 ^ ((~x4)>>23);
	XX7 = ?XOR(X7, XX6), % x7 ^= x6;
	{XX0, XX1, XX2, XX3, XX4, XX5, XX6, XX7}.

key_schedule2({X0,X1,X2,X3,X4,X5,X6,X7}) ->
	XX0 = ?ADD(X0, X7), % x0 += x7;
	XX1 = ?SUB(X1, ?XOR(XX0, ?SHL(?NOT(X7),19))), % x1 -= x0 ^ ((~x7)<<19);
	XX2 = ?XOR(X2, XX1), % x2 ^= x1;
	XX3 = ?ADD(X3, XX2), % x3 += x2;
	XX4 = ?SUB(X4, ?XOR(XX3, ?SHR(?NOT(XX2),23))), % x4 -= x3 ^ ((~x2)>>23);
	XX5 = ?XOR(X5, XX4), % x5 ^= x4;
	XX6 = ?ADD(X6, XX5), % x6 += x5;
	XX7 = ?SUB(X7, ?XOR(XX6, 16#0123456789ABCDEF)), % x7 -= x6 ^ 0x0123456789ABCDEF;
	{XX0, XX1, XX2, XX3, XX4, XX5, XX6, XX7}.
