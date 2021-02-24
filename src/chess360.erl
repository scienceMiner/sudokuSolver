%% @author ethancollopy
%% @doc @todo Add description to nq.


-module(chess360).
-import(lists,[member/2,zip/2]). 
-compile(export_all).

%% ====================================================================
%% All remaining white pieces are placed on the first rank.
%% The white king is placed somewhere between the two white rooks.
%% The white bishops are placed on opposite-colored squares.
%% The black pieces are placed equal-and-opposite to the white pieces. 
%% For example, if white's king is placed on b1, then black's king is placed on b8.

%% ====================================================================
%%-export([]).



%% ====================================================================
%% [rk,king,rk,qn,bp,bp,kn,kn] - these are the pieces
%% [_/1/b,_/2/w,_/3/b,_/4/w,_/5/b,_/6/w,_/7/b,_/8/w]
%%  first can be: r, b, kn, q,
%%  second can be: r (if 1 not r) , k (if 1 is r), b, kn, q 
%%  for each piece, randomly select such that it meets the rules
%% ====================================================================
%% [ { r/[1,2,3,4,5,6,7,8] } , { r,[]}

even(X) when X >= 0 -> (X band 1) == 0.
odd(X) when X > 0 -> not even(X).

domain(0)	->	[];
domain(N)	->	[N|domain(N-1)].

length1([X|Xs])		->
	1 + length1(Xs);
length1([])		-> 0;
length1(_)		-> 1.

domain() -> [1,2,3,4,5,6,7,8].
pieces() -> [r,r,k,kn,kn,b,b,q].

equiv([],[]) -> true;
equiv([],_) -> false;
equiv([X|Xs],Ys)	->
	lists:member(X, Ys),
	equiv(Xs,Ys--[X]).

kingInMiddle([],['r','k','r']) -> true;
kingInMiddle([],_) -> false;
kingInMiddle([{P,_}|Rest],R) -> 
			case  P of
				'r' -> kingInMiddle(Rest,[P|R]);
				'k' -> kingInMiddle(Rest,[P|R]);
				(_) -> kingInMiddle(Rest,R)
			end.

valid(A,B) ->
	even(A),odd(B);
valid(A,B) ->
	odd(A),even(B).

verifyBishop( [{'b',Val},{'b',Val2} ] ) ->
	valid(Val,Val2);
verifyBishop(_) -> false.

bishop([],Out) -> Out;
bishop([P|Rest],In) ->
	case P of 
		{'b',Value} -> bishop(Rest,[P|In]);
		(_) -> bishop(Rest,In)
	end.

checkBishop(In)		->
	verifyBishop(bishop(In,[])).


kingInMiddle(X) ->
	kingInMiddle(X,[]).
			

populate()	->	Domain = pieces(),
			Row = pieces(),
			Col = domain(),
			[ [{I,1},{I2,2},{I3,3},{I4,4},{I5,5},{I6,6},{I7,7},{I8,8}] ||  I <- Row, I2 <- Row--[I], I3 <- Row --[I,I2], I4 <- Row -- [I,I2,I3],
																		 									I5 <- Row -- [I,I2,I3,I4], I6 <- Row -- [I,I2,I3,I4,I5], 
																		   									I7 <- Row -- [I,I2,I3,I4,I5,I6], I8 <- Row -- [I,I2,I3,I4,I5,I6,I7] ,
																		  									equiv([I,I2,I3,I4,I5,I6,I7,I8],pieces()),
																		  									kingInMiddle([{I,1},{I2,2},{I3,3},{I4,4},{I5,5},{I6,6},{I7,7},{I8,8} ])  ,
																		   									checkBishop([{I,1},{I2,2},{I3,3},{I4,4},{I5,5},{I6,6},{I7,7},{I8,8} ])   ].
																		   									      
test()	->
checkBishop( [ {'b',2},{'k',2},{'kn',3},{'kn',4},{'r',5},{'r',6},{'q',7},{'b',8} ] ).
test2()	->
bishop( [ {'b',1},{'k',2},{'kn',3},{'kn',4},{'r',5},{'r',6},{'q',7},{'b',8} ] ,[]).
																		   									      


check( [] ) -> true;
check( [X] ) -> true;
check(  [ {X,Y}, {X2,Y2}  | Xs ] ) ->
	case  X =/= X2 of 
		true -> check( [ {X2,Y2}  | Xs ]);
		false -> false
	end.

filter([]) -> [];
filter([X]) -> [X];
filter( [X|Xs] )	->
	case check(X) of
		true	->	[X|filter(Xs)]; %	[lists:last([X])];
		false	->  filter(Xs)
	end.


solve()	 -> lists:usort(populate() ).

