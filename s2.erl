%% ---
%%---
-module(s2).
-compile(export_all).

remove(X,[X|Xs])	-> Xs;
remove(X,[Y|Xs])	-> [Y|remove(X,Xs)];
remove(_,[])		-> [].

%% final(X)	->	final(X,[]).
%% final([],Result)	->	Result;
%% final([X|Xs],R)		->	addAll(X,final(Xs,[addAll(X,allrotate(Xs))|R])).

cat([X|Xs],R)           ->      [X|cat(Xs,R)];
cat([],R)               ->      R.

sum([])		-> 0;
sum([X|T])  	-> X + sum(T).

accsum([],N)	-> N;
accsum([H|T],K)	-> accsum(T,K+H).

search(Key, [{Key,Val}|_]) -> {ok, Val};        %% (1)
search(Key, [_|T])         -> search(Key, T);    %% (2)
search(_, [])              -> error.             %% (3)


%% Input is the entry board

populate(Input)	->	Domain = [1,2,3,4,5,6,7,8,9],
			Row = [a,b,c,d,e,f,g,h,i],
			Col = [1,2,3,4,5,6,7,8,9],
			[{I,N,Domain} || I <- Row, N <- Col, not member2({I,N,Domain},Input) ].

%% Successively apply each constraint regardless of whether it is
%% row or col
clear(Type,[{R,C,Value}|Rest],Board)	->
	A	=	applyConstraint(Type,{R,C,Value},Board),
	clear(Type,Rest,A);
%%clear(box,[{R,C,Value}|Rest],Board)	->
%%	A	=	applyConstraint(box,{R,C,Value},Board),
%%	clear(box,Rest,A);
clear(_,[],B)			->	B.

%clearbox([{R,C,Value}|Rest],Board)	->
%	A	=	applyConstraint(box,{R,C,Value},Board,_),




applyConstraint(row,{R,C1,Value},[{R,C,Domain}|Rest])	->
	[{R,C,remove(Value,Domain)}|applyConstraint(row,{R,C1,Value},Rest)];
applyConstraint(row,{R,C,Value},[{R2,C2,D}|Rest])	->
	[{R2,C2,D}|applyConstraint(row,{R,C,Value},Rest)];
applyConstraint(col,{R1,C,Value},[{R2,C,Domain}|Rest])	->
	[{R2,C,remove(Value,Domain)}|applyConstraint(col,{R1,C,Value},Rest)];
applyConstraint(col,{R,C,Value},[{R2,C2,Domain}|Rest])	->
	[{R2,C2,Domain}|applyConstraint(col,{R,C,Value},Rest)];
applyConstraint(box,{R,C,Value},[{R1,C1,Domain}|Rest])
      when ((((R == a) or (R == b) or (R == c)) and ((R1 == a) or (R1 == b) or (R1 == c))
        or (((R == d) or (R == e) or (R == f)) and ((R1 == d) or (R1 == e) or (R1 == f)))
        or (((R == g) or (R == h) or (R == i)) and ((R1 == g) or (R1 == h) or (R1 == i))) )
     and  ( (((C == 1) or (C == 2) or (C == 3)) and ((C1 == 1) or (C1 == 2) or (C1 == 3)))
        or  (((C == 4) or (C == 5) or (C == 6)) and ((C1 == 4) or (C1 == 5) or (C1 == 6)))
        or  (((C == 7) or (C == 8) or (C == 9)) and ((C1 == 7) or (C1 == 8) or (C1 == 9)))) ) ->
	[{R1,C1,remove(Value,Domain)}|applyConstraint(box,{R,C,Value},Rest)];
applyConstraint(box,{R,C,Value},[{R2,C2,Domain}|Rest])	->
	[{R2,C2,Domain}|applyConstraint(box,{R,C,Value},Rest)];
applyConstraint(_,_,[])				->	[].

%% applying a box constraint
%%	apply this to board - say, after, testReduce().
%%

applyBoxConstraint({R,C,Value},Board)
	when length(Value) > 1	->
		D = uniqueList(formDomain(R,C,Board)),
		{R,C,clearIt(Value,D,Value)};
applyBoxConstraint({R,C,Value},_)	->	
		{R,C,Value}.

allBoxConstraints(Board)	->
	boxConstraint(Board,Board).

boxConstraint([Cell|RestOfBoard],Board)	->
		R = applyBoxConstraint(Cell,Board),
		boxConstraint(RestOfBoard,lists:append(removeCell(Cell,Board),[R]));
boxConstraint([],Board)	->
		Board.
	
removeCell({R,C,Value},[{R,C,Value}|Rest])	->
		Rest;	
removeCell({R1,C1,Value},[{R2,C2,Value}|Rest])	->
		[{R2,C2,Value}|removeCell({R1,C1,Value},Rest)];
removeCell({_,_,_},[])			->
		[].	
		
applyBoxConstraint([{R,C,Value}|RestOfBoard])	
	when length(Value) > 1	->
		D = uniqueList(formDomain(R,C,RestOfBoard)),
		%D = Value,
		%%[{R,C,clearIt(Value,D,Value)}|applyBoxConstraint(RestOfBoard)];
		[{R,C,clearIt(Value,D,Value)}|RestOfBoard];
applyBoxConstraint([{R,C,Value}|RestOfBoard])	->	
		%%[{R,C,Value}|applyBoxConstraint(RestOfBoard)];
		[{R,C,Value}|RestOfBoard];
applyBoxConstraint([])	->	[].

%% Domain is a list i.e. [4,5,6,8] of concatenated unique values in box
%% if [X|Xs] is [4,5,7] 7 is what we want the value to be set to
%% if [X|Xs] is [4,5,7,9] [7,9] is what we want the value to be set to
%%	s2:clearIt([3,5,8],[7,1,9,3,2,4,5,6]).

clearIt([X],Domain,V)	->
	case notMember(X,Domain) of
		true	->	[X]; %	[lists:last([X])];
		false	-> 	V
	end;
clearIt([X|Xs],Domain,V)	->
	case notMember(X,Domain) of
		true	->	[X|clearIt(Xs,Domain,[])]; % [lists:last([X])|clearIt(Xs,Domain,[])];
		false	-> 	clearIt(Xs,Domain,V)
	end.
%%clearIt([],D)		->	D.


clearThis(Values,Domain)	->	
	[float(V) || V <- Values, notMember(V,Domain) ].


	
notMember(X,[X|_])	->
	false;
notMember(X,[_|Ys])	->
	notMember(X,Ys);
notMember(_,[])		->
	true.

formDomain(R,C,[{R2,C2,Domain}|Rest]) ->
	case inBox(R,C,R2,C2,boxes()) of 
		true	->	flatten2([Domain|formDomain(R,C,Rest)]);
		false	->	flatten2(formDomain(R,C,Rest))
	end;
formDomain(_,_,[])	->		[].

	
flatten([[X|Xs]|Rest])	->
	[X|flatten([Xs|Rest])];
flatten([[]|Rest])	->	flatten(Rest);
flatten([X|Xs])		->	[X|Xs];
flatten([])		->	[].

flatten2([[X|Xs]|Rest])	->
	[X|flatten2([Xs|Rest])];
flatten2([[]|Rest])	->	flatten2(Rest);
flatten2([X|Xs])		->	[X|flatten2(Xs)];
flatten2([])		->	[].

testForm()	-> uniqueList(formDomain(a,2,[{a,1,[7]},{a,2,[3,5,8]},{a,3,[4,5,6,9]},{b,1,[2,4,5,9]},{b,2,[1,2,3,5]},{b,3,[1,2,4,5,9]},{c,1,[2,4,5,6]},{c,2,[2,3,5]},{c,3,[2,4,5,6]}])).

uniqueList([X|Xs])	->
	case notMember(X,Xs) of
		true	->	[X|uniqueList(Xs)];
		false	->	uniqueList(Xs)
	end;
uniqueList([])	->	[].

%% call this with boxes	
%%
inBox(R1,C1,R1,C1,[[_,_]|_]) ->
	false;
inBox(R1,C1,R2,C2,[[Rows,Cols]|Rest]) ->
	case (member(R1,Rows) and member(R2,Rows) and member(C1,Cols) and member(C2,Cols)) of
		true	->	true;
		false	->	inBox(R1,C1,R2,C2,Rest) 
	end;
inBox(_,_,_,_,[])	->
	false.
	

	

%% for box reduce
boxes()	->	[[[a,b,c],[1,2,3]],[[a,b,c],[4,5,6]],[[a,b,c],[7,8,9]],
		[[d,e,f],[1,2,3]],[[d,e,f],[4,5,6]],[[d,e,f],[7,8,9]],
		[[g,h,i],[1,2,3]],[[g,h,i],[4,5,6]],[[g,h,i],[7,8,9]] ].

testRow()	-> 	[{a,1,[1,2,3,4]},{b,2,[1,2,3,4]},{c,5,[1,2,3,4]}].
%testRow()	-> 	[{b,2,[1,2,3,4]},{c,5,[1,2,3,4]}].
getEntryBoard()	->	[{a,4,3},{a,8,1},{b,5,2}].

test(box,{R,C,Value},[{R1,C1,Domain}|_])
        when ((((R == a) or (R == b) or (R == c)) and ((R1 == a) or (R1 == b) or (R1 == c))
        or (((R == d) or (R == e) or (R == f)) and ((R1 == d) or (R1 == e) or (R1 == f)))
        or (((R == g) or (R == h) or (R == i)) and ((R1 == g) or (R1 == h) or (R1 == i))) )
       and  ( (((C == 1) or (C == 2) or (C == 3)) and ((C1 == 1) or (C1 == 2) or (C1 == 3)))
        or  (((C == 4) or (C == 5) or (C == 6)) and ((C1 == 4) or (C1 == 5) or (C1 == 6)))
        or  (((C == 7) or (C == 8) or (C == 9)) and ((C1 == 7) or (C1 == 8) or (C1 == 9)))) ) ->

        io:format("Can remove ~p from ~p ~n",[Value,Domain]);

test(box,{_,_,Value},[{_,_,Domain}|_]) ->

        io:format("CanNOT remove ~p from ~p ~n",[Value,Domain]).

% getEntryBoard standard 19 Oct 2009

getFullBoard1()	->	[{a,2,7},{a,4,2},{a,6,5},{a,8,3},{b,5,8},{b,7,9},{c,1,6},{c,3,9},{c,5,4},{d,4,5},{d,7,4},{e,1,4},{e,2,9},{e,8,5},{e,9,6},{f,3,1},{f,6,4},{g,5,5},{g,7,3},{g,9,8},{h,3,2},{h,5,3},{i,2,3},{i,4,9},{i,6,1},{i,8,2}].
getFullBoard()	->	[{a,1,7},{a,4,1},{a,9,2},{b,6,6},{b,8,8},{c,4,8},{c,7,1},{c,9,9},{d,3,7},{d,6,9},{d,8,1},{e,2,9},{e,3,3},{e,7,5},{e,8,4},{f,2,6},{f,4,4},{f,7,9},{g,1,3},{g,3,8},{g,6,4},{h,2,4},{h,4,3},{i,1,1},{i,6,5},{i,9,3}].
getEntryCons1()	->	[{a,4,3},{b,5,2}].

%% Note that constraints must be ordered by row
clearAll(Fixed,Variable)	-> 	
	A = clear(row,Fixed,Variable),
	B = clear(box,Fixed,A),
	clear(col,Fixed,B).

testReduce(Board)	-> 	
	A = clear(row,Board,populate(Board)),
	B = clear(box,Board,A),
	clear(col,Board,B).

testReduce1(Board)	-> 	
	A = clear(row,Board,populate(Board)),
	B = clear(box,Board,A),
	clear(col,Board,B).

integrate([{R1,C1,V}|Rest],[{R1,C2,D}|RestB])
        when    (C1 < C2)       ->
                [{R1,C1,V}|integrate(Rest,[{R1,C2,D}|RestB])];
integrate([{R1,C1,V}|Rest],[{R1,C2,D}|RestB])
        when    (C2 < C1)       ->
                [{R1,C2,D}|integrate([{R1,C1,V}|Rest],RestB)];
integrate([{R1,C1,V}|Rest],[{R2,C2,D}|RestB])
        when    (R1 < R2)       ->
                [{R1,C1,V}|integrate(Rest,[{R2,C2,D}|RestB])];
integrate([{R1,C1,V}|Rest],[{R2,C2,D}|RestB])
        when    (R1 > R2)       ->
                [{R2,C2,D}|integrate([{R1,C1,V}|Rest],RestB)];
integrate([R|Rest],[])          ->
        [R|Rest];
integrate([],[R|Rest])          ->
        [R|Rest].

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
	io:format(" "),
	spaceInsert(X-1);
spaceInsert(0) -> 0.

start(B)       ->
        display(integrate(B,populate(B))).

init()       ->
        display(integrate(getFullBoard(),populate(getFullBoard()))).
init1()       ->
        display(integrate(getFullBoard1(),populate(getFullBoard1()))).
testint(B)       ->
        display(integrate(testReduce(B),B)).
testint2(B)      ->
        %% display(allBoxConstraint(integrate(getFullBoard(),testReduce()))).
        display(allBoxConstraints(integrate(B,testReduce(B)))).

testint3(B)      ->
        A = allBoxConstraints(integrate(B,testReduce(B))),
	display(allBoxConstraints(integrate(getFixed(A),clearAll(getFixed(A),getVariable(A))))).

testint3m2(B)      ->
        A = allBoxConstraints(integrate(B,testReduce(B))),
	B = allBoxConstraints(integrate(getFixed(A),clearAll(getFixed(A),getVariable(A)))),
	display(allBoxConstraints(integrate(getFixed(B),clearAll(getFixed(B),getVariable(B))))).

testint4()      ->
        A = allBoxConstraints(integrate(getFullBoard1(),testReduce1(getFullBoard1()))),
	B = allBoxConstraints(integrate(getFixed(A),clearAll(getFixed(A),getVariable(A)))),
	C = allBoxConstraints(integrate(getFixed(B),clearAll(getFixed(B),getVariable(B)))),
	D = allBoxConstraints(integrate(getFixed(C),clearAll(getFixed(C),getVariable(C)))),
	E = allBoxConstraints(integrate(getFixed(D),clearAll(getFixed(D),getVariable(D)))),
	F = allBoxConstraints(integrate(getFixed(E),clearAll(getFixed(E),getVariable(E)))),
	G = allBoxConstraints(integrate(getFixed(F),clearAll(getFixed(F),getVariable(F)))),
	I = allBoxConstraints(integrate(getFixed(G),clearAll(getFixed(G),getVariable(G)))),
	J = allBoxConstraints(integrate(getFixed(I),clearAll(getFixed(I),getVariable(I)))),
	K = allBoxConstraints(integrate(getFixed(J),clearAll(getFixed(J),getVariable(J)))),
	L = allBoxConstraints(integrate(getFixed(K),clearAll(getFixed(K),getVariable(K)))),
	M = allBoxConstraints(integrate(getFixed(L),clearAll(getFixed(L),getVariable(L)))),
	N = allBoxConstraints(integrate(getFixed(M),clearAll(getFixed(M),getVariable(M)))),
	O = allBoxConstraints(integrate(getFixed(N),clearAll(getFixed(N),getVariable(N)))),
	P = allBoxConstraints(integrate(getFixed(O),clearAll(getFixed(O),getVariable(O)))),
	Q = allBoxConstraints(integrate(getFixed(P),clearAll(getFixed(P),getVariable(P)))),
	R = allBoxConstraints(integrate(getFixed(Q),clearAll(getFixed(Q),getVariable(Q)))),
	S = allBoxConstraints(integrate(getFixed(R),clearAll(getFixed(R),getVariable(R)))),
	T = allBoxConstraints(integrate(getFixed(S),clearAll(getFixed(S),getVariable(S)))),
	U = allBoxConstraints(integrate(getFixed(T),clearAll(getFixed(T),getVariable(T)))),
	display(U).
	
testint5()      ->
        A = allBoxConstraints(integrate(getFullBoard1(),testReduce1(getFullBoard1()))),
	display(A).

isList([])	->	false;
isList([_])	->	false;
isList([_|_])	->	true;
isList(_)	->	false.

isSmall([])	->	false;
isSmall([_])	->	true;
isSmall([_|_])	->	false;
isSmall(_)	->	true.

isElem([])	->	false;
isElem([_])	->	true;
isElem([_|_])	->	false;
isElem(_)	->	true.

getFixed([])			->	[];	
getFixed([{R,C,[Value]}|Rest])	->	[{R,C,Value}|getFixed(Rest)];
getFixed([{R,C,Value}|Rest])	->
	case ((isSmall(Value))) of 
		true	-> [{R,C,Value}|getFixed(Rest)];
		false	-> getFixed(Rest)
	end.


getVariable([])			->	[];
getVariable([{R,C,Value}|Rest])
	when length(Value) > 1 ->
	[{R,C,Value}|getVariable(Rest)];
getVariable([{_,_,_}|Rest]) ->
	getVariable(Rest).


%
%	For each cell
%		choose a value
%		remove this from all multi-values in box,cell,row
%	integrate board
%	For each multi-cell - choose value
%			remove from all others in row,col,box
%	
%
%
%
%
%


allDiff(X,[X])  -> false;
allDiff(X,[Y]) when not (X == Y)  -> true;
allDiff(X,[X|_])-> false;
allDiff(X,[_|T])-> allDiff(X,T).  %% X =/= H has no effect here

unique([])      -> true; %% is this strictly necessary?
unique([_])     -> true;
unique([X|T])   ->
        case allDiff(X,T) of
                true -> unique(T);
                false -> false
        end.

%

% add some sort of unique row
% attempt to populate centre box first

%
	
runit()		->
	display(integrate(testReduce(getFullBoard()),getFullBoard())).

%%	CALL will be:
%%	testReduce()	-> reduce(populate(getEntryBoard()),getEntryBoard()).

%sameBox({I,N,_},{J,K,_})	->
%				pairMember({{I,J},{N,K}},boxes()).

%pairMember({{I,J},{N,K}},[[Rows,Cols]|Rest] ) when	
%	( member(I,Rows) and member(J,Rows) and member(N,Cols) and member(K,Cols) ) ->	true;
%pairMember({{I,J},{N,K}},[[_,_]|Rest] )	->
%			pairMember({{I,J},{N,K}},Rest);
%pairMember({{I,J},{N,K}},[])			->	
%			false.
			

%pairMember({{I,J},{N,K}},[[[I,J,_],[N,K,_]]|Rest] )	->	true; 

map(_, [])     -> [];
map(F,  [H|T]) -> [F(H)|map(F, T)].

member2({I,N,_},[{I,N,_}|_])	-> true;
member2({I,N,D},[_|T])		-> member2({I,N,D},T);
member2({_,_,_},[])		-> false.

member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_, [])    -> false.

% 2434/081109/49

solveIt(Board)      ->
        A = clearDomainRow(clearDomainCol(allBoxConstraints(integrate(Board,testReduce(Board))))),
	loop(A).

solveIt1(Board)      ->
        A = clearDomainRow(clearDomainCol(allBoxConstraints(integrate(Board,testReduce1(Board))))),
	loop(A).

loop(Board)	->
   B = clearDomainRow(clearDomainCol(allBoxConstraints(integrate(getFixed(Board),clearAll(getFixed(Board),getVariable(Board)))))),
	case same(B,Board) of 
		true	->	B; % display(B); % display2(B);
		false	->	loop(B)	
	end.

solveMain(Board)      ->
        A = clearDomainRow(clearDomainCol(allBoxConstraints(integrate(Board,testReduce1(Board))))),
	loopMain(A).

loopMain(Board)	->
   B = clearDomainRow(clearDomainCol(allBoxConstraints(integrate(getFixed(Board),clearAll(getFixed(Board),getVariable(Board)))))),
	case same(B,Board) of 
		true	->	display(B); % display(B); % display2(B);
		false	->	loopMain(B)	
	end.


same([{R,C,Value}|Rs],[{R,C,Value}|Rs2])	->
	same(Rs,Rs2);
same([{_,_,_}|_],[{_,_,_}|_])	->
	false;
same([],[])					->
	true;
same([],_)					->	false;
same(_,[])					->	true.


% rule to clear entries from a row, column or box
% previous clear row clears already fixed values from the domain

% this rule clears not possible from the domain

clearDomain(Board)	->
	clearDomain(Board,Board).
clearDomainRow(Board)	->
	clearDomain(row,Board,Board).
clearDomainCol(Board)	->
	clearDomain(col,Board,Board).

clearDomain([{R,C,Value}|Rest],Board)	->
	case ((isList(Value)) ) of 
		true	-> [{R,C,clearExtras(R,C,Value,Board)}|clearDomain(Rest,Board)];
		false	-> [{R,C,Value}|clearDomain(Rest,Board)]
	end;
clearDomain([],_)			->	[].

clearDomain(Type,[{R,C,Value}|Rest],Board)	->
	case ((isList(Value)) ) of 
		true	-> [{R,C,clearExtras(Type,R,C,Value,Board)}|clearDomain(Type,Rest,Board)];
		false	-> [{R,C,Value}|clearDomain(Type,Rest,Board)]
	end;
clearDomain(_,[],_)			->	[].

clearExtras(Type,R,C,Value,Board)			->
	A = flatten2(createOutsideDomain(Type,R,C,Board)), % A - domain of row apart from current cell
	removeExtras(Value,A,Value).


clearExtras(R,C,Value,Board)			->
	A = flatten2(createOutsideDomain(row,R,C,Board)), % A - domain of row apart from current cell
        % io:format("Outside Domain ~p , ~p is ~p ~n",[R,C,A]),
	B = removeExtras(Value,A,Value),
        % io:format("Extras Removed from ~p are ~p ~n",[Value,B]),
	B.

removeExtras([V|Vs],A,InputValue)	->
	case (not member(V,A)) of
		true	->	[V|removeExtras(Vs,A,[])];
		false	->	removeExtras(Vs,A,InputValue)
	end;
removeExtras([],_,IV)	->	IV;
removeExtras(_,_,IV)	->	IV.


createOutsideDomain(row,_,_,[])	->		[];
createOutsideDomain(row,R,C,[{R,C,_}|Rest])	->
	createOutsideDomain(row,R,C,Rest);
createOutsideDomain(row,R,C,[{R,_,Value}|Rest])	->
	[Value|createOutsideDomain(row,R,C,Rest)];
createOutsideDomain(row,R,C,[{_,_,_}|Rest])	->
	createOutsideDomain(row,R,C,Rest);
createOutsideDomain(col,_,_,[])	->		[];
createOutsideDomain(col,R,C,[{R,C,_}|Rest])	->
	createOutsideDomain(col,R,C,Rest);
createOutsideDomain(col,R,C,[{_,C,Value}|Rest])	->
	[Value|createOutsideDomain(col,R,C,Rest)];
createOutsideDomain(col,R,C,[{_,_,_}|Rest])	->
	createOutsideDomain(col,R,C,Rest).

%	For a row OR col within a box
%	if a domain member is NOT in other rows or columns
%	can attempt to remove it from the other the row or column
%	outside of the box


%	Form Row Domain for the box of affected cell
%	[1,2,4,5] and [1,2,3,5] and [3,4,5] are in middle row of box A
%	so we have [1,2,3,4,5] a box row domain
%	Domain of other rows of box are: [1,2,4,5,6,7,8,9]
%	so candidate is [3]
%	so this can be removed from rows in the box outside the box

% 	locked candidates

lockedCandidatesRow(Board)	->
	lockedCandidates(row,Board,Board).
lockedCandidatesCol(Board)	->
	lockedCandidates(col,Board,Board).

% Need to update Board in Rest as well move on.
lockedCandidates(Type,[{R,C,Value}|Rest],Board)	->
	A = formBlockDomain(Type,{R,C,Value},Board),
	lockedCandidates(Type,Rest,A);
lockedCandidates(_,[],B)			->	B.


formBlockDomain(Type,{R,C,_},Board)	->
        % io:format("NEW formBlockDomain for ~n"),
	%  display(Board),
	Box = getBox(R,C,boxes()),
	BD = getBoxDomain(Type,R,C,Box,Board),
	BND = getBoxNonDomain(Type,R,C,Box,Board),
	case BND == [] of
		true	->	Board;
		false	->	io:format("BoxDom ~p and BoxNonDom ~p ~n",[BD,BND]),
				% BoxDom [2,4] and BoxNonDom []
				Candidate = lists:usort(BD) -- lists:usort(BND),
        			% io:format("Candidate ~p ~n",[Candidate]),
				clearOutsideBox(Type,Candidate,R,C,Board)
		end.
	
% if anything is a member of BD but Not BND
% clear from rest of row

toList([X])	->	[X];
toList([X|Xs])	->	[X|Xs];
toList([])	->	[];
toList(E)	->	[E].

clearOutsideBox(_,[],_,_,Board)		->	Board;
clearOutsideBox(row,Candidate,R,C,[{R,C1,Value}|Rest])	->
	case	inSideBox(row,R,R,C,C1)	of % and (isElem(Value)) of
		%false	->	[{R,C1,Value--Candidate}|clearOutsideBox(row,Candidate,R,C,Rest)];
		false	-> 
			V1 = listrem(Value,Candidate),
			io:format("ROW removal ROW ~p COL ~p Value ~p Candidate ~p listrem ~p ~n",[R,C1,Value,Candidate,V1]),
			[{R,C1,V1}|clearOutsideBox(row,Candidate,R,C,Rest)];
		%false	->	[{R,C1,Value}|clearOutsideBox(row,Candidate,R,C,Rest)];
		true	->	[{R,C1,Value}|clearOutsideBox(row,Candidate,R,C,Rest)]
	end;
clearOutsideBox(row,Candidate,R,C,[{R1,C1,Value}|Rest])	->
	[{R1,C1,Value}|clearOutsideBox(row,Candidate,R,C,Rest)];

clearOutsideBox(col,Candidate,R,C,[{R1,C,Value}|Rest])	->
	case	inSideBox(col,R,R1,C,C)	of % and (isElem(Value)) of
	false	->	
		V1 = listrem(Value,Candidate),
		io:format("COL removal ROW ~p COL ~p Value ~p Candidate ~p listrem ~p ~n",[R1,C,Value,Candidate,V1]),
		[{R1,C,V1}|clearOutsideBox(col,Candidate,R,C,Rest)];
		% io:format("COL removal ROW ~p COL ~p Value ~p Candidate ~p listrem ~p ~n",[R,C,Value,Candidate,V1]);
		%false	->	[{R1,C,Value}|clearOutsideBox(col,Candidate,R,C,Rest)];
		true	->	[{R1,C,Value}|clearOutsideBox(col,Candidate,R,C,Rest)]
	end;
clearOutsideBox(col,Candidate,R,C,[{R1,C1,Value}|Rest])	->
	[{R1,C1,Value}|clearOutsideBox(col,Candidate,R,C,Rest)];
clearOutsideBox(_,_,_,_,[])	-> [].

inSideBox(_,R,R1,C,C1)	->
	B1 = getBox(R,C,boxes()),
	B2 = getBox(R1,C1,boxes()),
	(B1 == B2).	

getBox(R,C,Boxes)	->
	flatten2([ [Rows,Cols] || [Rows,Cols] <- Boxes, member(R,Rows), member(C,Cols) ]).

getOutsideBox(row,R,C,Boxes)	->
	flatten2([ [Rows,Cols] || [Rows,Cols] <- Boxes, member(R,Rows), not(member(C,Cols)) ]);
getOutsideBox(col,R,C,Boxes)	->
	flatten2([ [Rows,Cols] || [Rows,Cols] <- Boxes, not(member(R,Rows)), member(C,Cols) ]).

%getBoxDomain(row,R,C,[Rows,Cols],[{R,C,Value}|Rest])	->
%	[{R,C,Value}|getBoxDomain(row,R,C,[Rows,Cols],Rest)].
getBoxDomain(row,R1,C,[Rows,Cols],Rest)	->
	A = [{R,C1,Value} || {R,C1,Value} <- Rest, R == R1, member(C1,Cols)],
	formListDomain(A);
getBoxDomain(col,R,C1,[Rows,Cols],Rest)	->
	A = [{R1,C,Value} || {R1,C,Value} <- Rest, C == C1, member(R1,Rows)],
	formListDomain(A).
getBoxNonDomain(row,R1,C,[Rows,Cols],Rest)	->
	A = [{R,C1,Value} || {R,C1,Value} <- Rest,member(C1,Cols),member(R,Rows),not(R1 == R)],
	formListDomain(A);
getBoxNonDomain(col,R,C1,[Rows,Cols],Rest)	->
	A = [{R1,C,Value} || {R1,C,Value} <- Rest,member(C,Cols),member(R1,Rows),not(C1 == C)],
	formListDomain(A).

formListDomain([{_,_,Value}|Rest])	->
	case	(isList(Value))	of
		true	->	(flatten2([Value|formListDomain(Rest)]));
		false	->	formListDomain(Rest)
	end;
formListDomain([])	->	[].

% s2:formBlockDomain(row,{d,4,[1,2,3]},[{a,1,4},{a,2,[1,3]},{a,3,[2,3,4]},{a,4,[4,5,3,2]},{b,6,[1,2]},{d,4,[1,2,3]},{d,5,[7,8,9]},{e,5,[4,3,5]},{f,6,[1,9]},{d,9,[3,4,5]}]).



hiddenPairs(Board)                      ->
        NBoard1 = hiddenPairs(row,Board,Board),
        NBoard2 = hiddenPairs(col,NBoard1,NBoard1).%,
%        hiddenPairs(box,NBoard2,NBoard2).

hiddenPairs(Type,[{R,C,Value}|Rest],Board)      ->
        case existElsewhere(Type,R,C,Value,Board)       of
                true    ->      hiddenPairs(Type,Rest,removeFromBoard(Type,R,C,Value,Board));
                false   ->      hiddenPairs(Type,Rest,Board)
        end;
hiddenPairs(Type,[],Board)      ->      Board.

existElsewhere(row,R,C,Value,[{R,C1,Value}|_])
        when    not (C == C1)   ->      true;
existElsewhere(row,R,C,Value,[{R,C1,Value1}|Rest])     
        when    not (C == C1)   ->      existElsewhere(row,R,C,Value,Rest);
existElsewhere(col,R,C,Value,[{R1,C,Value}|_])
        when    not (R == R1)   ->      true;
existElsewhere(col,R,C,Value,[{R1,C,_}|Rest])     
        when    not (R == R1)   ->      existElsewhere(col,R,C,Value,Rest);
existElsewhere(_,_,_,_,[])        ->      false.


removeFromBoard(row,R,C,Value,[{R,C1,Value1}|Rest])	
	when	not (C == C1)	->
		[{R,C1,clearDiffEntries(Value,Value1)}|removeFromBoard(row,R,C,Value,Rest)];
removeFromBoard(row,R,C,Value,[{_,_,_}|Rest])	->
	removeFromBoard(row,R,C,Value,Rest);
removeFromBoard(col,R,C,Value,[{R1,C,Value1}|Rest])	
	when	not (R == R1)	->
		[{R,C,clearDiffEntries(Value,Value1)}|removeFromBoard(col,R,C,Value,Rest)];
removeFromBoard(col,R,C,Value,[{_,_,_}|Rest])	->
	removeFromBoard(col,R,C,Value,Rest);
removeFromBoard(_,_,_,_,[])			->	[].

clearDiffEntries(List1,List1)	->
		List1;
clearDiffEntries(List1,List2)	->
		List2 -- List1.


listRemove(X,[])	-> X;
listRemove(X,[X])	-> [];
listRemove(X,[X|_])	-> [];
listRemove(X,[_])	-> X;
listRemove(X,[_|_])	-> X;
listRemove([X],[])	-> [X];
listRemove([X],[X])	-> [];
listRemove([X],[_])	-> [X];
listRemove([X],[X|_])	-> [];
listRemove([X],[_|Ys])	-> listRemove(X,Ys);
listRemove([X|Xs],[X|Ys])	-> listRemove(Xs,Ys);
listRemove([X|Xs],[Y|Ys])	-> [X|listRemove(Xs,[Y|Ys])];
listRemove([],[_|_])	-> [].


listRemove2([X],[X|_])	->	[];
listRemove2([X|Xs],[Y|Ys])	->	lists:append(listRemove2(Xs,[Y|Ys]),listRemove2(X,Ys));	
listRemove2([],[_|_])	->	[];
listRemove2([X],[])	->	[X];
listRemove2(X,_)		->	X.



% Xs,Ys - remove any occurrences of Ys from Xs providing result is non-empty

listrem(X,Y)	->
	case isFullList(X) and not(sameList(X,Y)) of
		true	->	X -- Y;
		false	->	X
	end.

isFullList([])	->	false;
isFullList([_])	->	false;
isFullList([_|_])	->	true;
isFullList(_)	->	false.

% same for contained lists and false if first arg is non-list
sameList([],[])	->	true;
sameList([X|Xs],Ys)	->
	case lists:member(X,Ys) of
		true	->	sameList(Xs,Ys--[X]);
		false	->	false
	end;
sameList(_,_)	->	false.


isFinished(Board)	->	
	A = [{R,C,Value} || {R,C,Value} <- Board, not(isElem(Value))],
	isEmpty(A).

isEmpty([])		->	true;
isEmpty(_)		->	false.


%select([],IB,Count)			->	select(IB,IB,Count);	% used for initialbacktracking scenario
%select(Board,InitialBoard,Count)	->	
%	case isFinished(Board) and (length(Board) == 81) of
%		true	-> 	Board;
%		false	->		
%			CP = resolve(CurrentPiece,Board,Count), % chooses a value for first piece
%			Test = solveIt1(CP),
%			NewCount = validate(Test,Board,Count),		% validates Board,Test
%				% returns either a board or an improvedBoard or [] if error
%			select(VB,InitialBoard,Count+5);  
%				% could give [] as VB and define select([],InitialB) case
%				% Add some sort of count so that the [] call knows where to go.
%		end.
%							% repeat selection process whilst 
%
%resolve(B,0)				->	B;
%resolve([{R,C,Value}|Rest],Count)	->
%	case	not(isElem(Value)) and Count > 0 of
%		true	->	[{R,C,filter(Value)}|resolve(Rest,Count-1)];
%		false	->	[{R,C,Value}|resolve(Rest,Count)]
%	end.
%
%filter(




%
%validate(Board,OriginalBoard)	->
%	case isCorrect(Board) of
%		true	->	Board;
%		false	->	OriginalBoard
%	end;
%



search1([],B)			->	display(B);
search1([{R,C,Val}|Rest],Board)	->
        case isElem(Val) of
                true    ->      search1(Rest,Board);
                false   ->      LOP = formLOP({R,C,Val}),
				NewBoard = applyToBoard(hd(LOP),Board),
				io:format(" ~p replaced in Row ~p Col ~p ~n",[hd(LOP),R,C]),
				% display(NewBoard),
                               	Board2 = solveIt1(NewBoard), 
                                case isFinished(Board2) and (length(Board2) == 81) and check(Board2) of
					true	->	io:format("isFinished ~n"),
							display(Board2);
        				false	->	io:format("Row ~p Col ~p Remove ~p from ~p ~n",[R,C,access(hd(LOP)),Val]),
							search1([{R,C,Val--[access(hd(LOP))]}|Rest],Board)
				end
	end.
						

% ADD BOARDS	


search2([],B,_)			->	B; % display(B);
search2([{R,C,Val}|Rest],Board,InputLOP)	->
        case isElem(Val) of
                true    ->      search2(Rest,Board,InputLOP);
                false   ->      LOP = formLOP({R,C,Val},Board),
				io:format("List of possibilities ~p ~n",[pieces(LOP)]),
				% display(sortDisp(Board)),
				NewLOP = cat(LOP,InputLOP),
				NewBoard = applyToBoard(hdpiece(LOP),Board),
				io:format(" ~p replaced1 in Row ~p Col ~p ~n",[hdpiece(LOP),R,C]),
				display(sortDisp(NewBoard)),
				io:format(" displayed "),
                               	Board2 = solveIt1(NewBoard), 
                                case (length(Board2) == 81) and check(Board2) of
					true	->	
						case	isFinished(Board2) of
							true	->	Board2; % display(Board2);
							false	->	search2(Rest,Board2,NewLOP)
						end;
        				false	->	
						io:format(" backtrack from Row ~p Col ~p ~n",[R,C]),
						% backtrack
						search3(Rest,Board,NewLOP--[hd(LOP)])
				end
	end.

search3(Pieces,Board,[{{R,C,Val},_}|LOP])	->
		NewBoard = applyToBoard({R,C,Val},Board),
		io:format(" backtrack with R ~p C ~p Val ~p ~n",[R,C,Val]),
		display(sortDisp(NewBoard)),
		Board2 = solveIt1(NewBoard),
                case (length(Board2) == 81) and check(Board2) of
			true	->	
				case	isFinished(Board2) of
					true	->	Board2; % display(Board2);
					false	->	search2(Pieces,Board2,LOP)
				end;
        		false	->	
				io:format(" backtrack again ~p ~n",[pieces(LOP)]),
				%io:format(" lop ~p ~n",[bdpiece(LOP)]),
				% backtrack again - but have the same board!!
				%io:format(" lopBoard ~p ~n",[bdpiece(LOP)]),
				case (isEmpty(bdpiece(LOP))) of 
					true	-> search3(Pieces,Board,LOP);
					false	-> search3(Pieces,bdpiece(LOP),LOP)
				end
		end;
search3(P,B,[])	->	search2(P,B,[]).
		

hdpiece([{{R,C,V},B}|Rs])	->	{R,C,V};
hdpiece([])			->	[].

bdpiece([{{R,C,V},B}|Rs])	->	B;
bdpiece([])			->	[].

pieces([])			->	[];	
pieces([{{R,C,Val},_}|Rest])	->	[{R,C,Val}|pieces(Rest)].


% need to choose pieces with smallest domain
formLOP({R,C,[]},_)	->		[];
formLOP({R,C,[V|Vs]},Board)	->	[{{R,C,V},Board}|formLOP({R,C,Vs},Board)].


formLOP({R,C,[]})	->	[];
formLOP({R,C,[V|Vs]})	->	[{R,C,V}|formLOP({R,C,Vs})].

applyToBoard({R,C,V},[])			->	[];
applyToBoard({R,C,V},[{R,C,V1}|Rest])		-> 	[{R,C,V}|Rest]; 
applyToBoard({R,C,V},[{R1,C1,Val}|Rest])	-> 	[{R1,C1,Val}|applyToBoard({R,C,V},Rest)].

access({R,C,V})		->	V.


start1(Board)	->
	A = solveIt1(Board),
	search1(A,A).

start2(Board)	->
	A = solveIt1(Board),
	A1 = sortFewest(A),
	B = search2(A1,A1,[]),
	display(sortDisp(B)).

start3(Board)	->
	A = solveIt1(Board),
	A1 = sortFewest(A),
	B = search2(A1,A1,[]),
        io:format("Next Search ~n"),
	B1 = sortFewest(B),
	C = search2(B1,B1,[]),
	display(sortDisp(B)),
        io:format("Start Search ~n"),
	display(sortDisp(C)).

check(Board)	->
	valid(row,Board,Board) and
	valid(col,Board,Board) and
	valid(box,Board,Board).

valid(_,[],_)				->	true;
valid(Type,[{R,C,B}|Rest],Board)	->
	case validUnit(Type,{R,C,B},Board) of 
		true	-> valid(Type,Rest,Board);
		false	-> false
	end.


validUnit(_,_,[])	->			true;
validUnit(row,{R,C,Val},[{R,C,_}|Rest])	->
	% same row,same col	-	ignore
	validUnit(row,{R,C,Val},Rest);
validUnit(row,{R,C,Val},[{R,_,Val1}|Rest])	->
	% same row,diff col
	case different(Val,Val1) of 
		true	->	validUnit(row,{R,C,Val},Rest);
		false	->	false
	end;
validUnit(row,{R,C,Val},[{_,_,_}|Rest])	->
	validUnit(row,{R,C,Val},Rest);

validUnit(col,{R,C,Val},[{R,C,_}|Rest])	->
	% same row,same col	-	ignore
	validUnit(col,{R,C,Val},Rest);
validUnit(col,{R,C,Val},[{_,C,Val1}|Rest])	->
	% same col,diff row
	case different(Val,Val1) of 
		true	->	validUnit(col,{R,C,Val},Rest);
		false	->	false
	end;
validUnit(col,{R,C,Val},[{_,_,_}|Rest])	->
	validUnit(col,{R,C,Val},Rest);

validUnit(box,{R,C,Val},[{R,C,_}|Rest])	->
	% same row,same col	-	ignore
	validUnit(box,{R,C,Val},Rest);
validUnit(box,{R,C,Val},[{R1,C1,Val1}|Rest])	->
	case inSideBox(box,R,R1,C,C1)	of
		true	->
			% same row,diff col
			case different(Val,Val1) of
				true	->		validUnit(box,{R,C,Val},Rest);
				false	->		false
			end;
		false	->
			validUnit(box,{R,C,Val},Rest)
	end.

different(V,V1)	->
	case	isElem(V)	and	isElem(V1)	of
	true	->
			case	V == V1	of
			true	->	false;
			false	->	true
			end;
	false	->	true	
	end.


sortFewest(R)	->	sortFewest(R,[]).

sortFewest([{R,C,V},{R1,C1,V2}|Rest],OrderedList)		->
	case	length1(V) > length1(V2)	of
		true	->	sortFewest([{R1,C1,V2}|Rest],insert({R,C,V},OrderedList));	
		false	-> 	sortFewest([{R,C,V}|Rest],insert({R1,C1,V2},OrderedList))
	end;
sortFewest([{R,C,V}],OrderedList)	->	insert({R,C,V},OrderedList).

insert({R,C,V},[{R1,C1,V1}|Rest])	->
	case	length1(V) > length1(V1)	of
		true	->	[{R1,C1,V1}|insert({R,C,V},Rest)];	
		false	-> 	[{R,C,V},{R1,C1,V1}|Rest]
	end;
insert({R,C,V},[])	->	[{R,C,V}].

%	prolog length
%	length([],0).
%	length([X|Xs],Y+1):-
%		length(Xs,Y).
	
length1([X|Xs])	->
	1 + length1(Xs);
length1([])	->	0;
length1(_)	->	1.

sortDisp(R)	->	sortDisp(R,[]).

sortDisp([],R)	->	R;
sortDisp([{R,C,V}|Rest],OrderedList)		->
	A = insertDisp({R,C,V},OrderedList),
	sortDisp(Rest,A).


insertDisp({R,C,V},[{R1,C1,V1}|Rest])	->
	case	R > R1	of
		true	->	[{R1,C1,V1}|insertDisp({R,C,V},Rest)];	
		false	-> 	
			case R == R1	of
				true	->
					case C > C1 of 
						true	-> [{R1,C1,V1}|insertDisp({R,C,V},Rest)];	
						false	-> [{R,C,V}|insertDisp({R1,C1,V1},Rest)]
					end;
				false	->	[{R,C,V}|insertDisp({R1,C1,V1},Rest)]
			end
	end;
insertDisp({R,C,V},[])	->	[{R,C,V}].
	


recurse(Board)	->
	recurse(Board,0).

recurse(B,N)	->
	case N rem 100000 == 0 of 
		true	->	
        			io:format("Board Number ~p ~n",[N]),
				C = cat(B,B),
				recurse(C,N+1);
		false	->	C = cat(B,B),
				recurse(B,N+1)
	end.

%	For each Option after sortFewest
%	Fill all boards
%	solve from this point
%	remove those that do not solve
%	stop when solution reached
%	

start4(Board)	->
	A = solveIt1(Board),
	A1 = sortFewest(A),
        io:format("Next Search ~n"),
	B = searchL(A1,A1),
	display(sortDisp(A1)).


searchL([],B)			->	B; % display(B);
searchL([{R,C,Val}|Rest],Board)	->
        case isElem(Val) of
                true    ->      searchL(Rest,Board);
                false   ->      NewBoards = insertAllBoards({R,C,Val},Board), % will now have say 3 brds
				searchAll(Rest,NewBoards)	
	end.

%	For each member of Boards
%	
searchAll([{R,C,Val}|Rest],Boards)	->
	case	isElem(Val)	of
		true	->	searchAll(Rest,Boards);
		false	->	NewBoards = insertAllBoardsSquared({R,C,Val},Boards),
				searchAll(Rest,NewBoards)
	end.

insertAllBoards({R,C,[V|Vs]},Board)	->
	TryBoard = applyToBoard({R,C,V},Board),
        Board2 = solveIt1(TryBoard),
	case (length(Board2) == 81) and check(Board2) of
			false	->	insertAllBoards({R,C,Vs},Board); 
			true	->	
				case	(isFinished(Board2))	of
					true	->	display(sortDisp(Board2));
					false	->	[Board2|insertAllBoards({R,C,Vs},Board)] 
				end 
	end;
insertAllBoards(_,[])			->	[];
insertAllBoards({R,C,[]},_)			->	[].

insertAllBoardsSquared({R,C,Val},[B|Rds])	->
	BoardSet1 =	insertAllBoards({R,C,Val},B),
	BoardSetRest = insertAllBoardsSquared({R,C,Val},Rds),
	cat(BoardSet1,BoardSetRest);
insertAllBoardsSquared(_,[])			->	[].


	
