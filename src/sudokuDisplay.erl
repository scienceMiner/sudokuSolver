%% @author ethancollopy
%% @doc @todo Add description to nq.


-module(sudokuDisplay).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
%%-export([]).

display2([{R1,C,Value}|Rest])	->
	io:format("Row ~p Col ~p Value ~p ~n",[R1,C,Value]),
	display2(Rest);
display2([])			->	[].

display([{R1,_,V},{R2,C2,D}|Rest])
        when    (R1 == R2)      ->
	io:format("| "),
        itemDisplay(V), % 
        preSpaceInsert(V),
        display([{R2,C2,D}|Rest]);
display([{R1,_,V},{R2,C2,D}|Rest])
        when    (R1 < R2)       ->
	io:format("| "),
        itemDisplay(V),
        io:format("~n"),
        io:format("-------------------------------------------------------------------------------------- ~n"),
        display([{R2,C2,D}|Rest]);
display([{_,_,V}])      ->
	io:format("| "),
        itemDisplay(V),
        io:format(" ~n ");
display([])     ->
        true.

itemDisplay([X|Xs]) ->
	io:format("~p",[X]),
	itemDisplay(Xs);
itemDisplay([]) -> [];
itemDisplay(D) -> 
	io:format("~p",[D]).

preSpaceInsert([X|Xs])	->
	A = length([X|Xs]),
	spaceInsert(9-A);
preSpaceInsert(_)	->
	spaceInsert(8).

spaceInsert(X) 
	when X > 0 ->
	io:format("   "),
	spaceInsert(X-1);
spaceInsert(0) -> 0.

