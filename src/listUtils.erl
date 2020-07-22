%% @author ethancollopy
%% @doc @todo Add description to ercUtils.

-module(listUtils).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
%%-export([]).

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

%	prolog length
%	length([],0).
%	length([X|Xs],Y+1):-
%		length(Xs,Y).
	
length1([X|Xs])	->
	1 + length1(Xs);
length1([])	->	0;
length1(_)	->	1.


isEmpty([])		->	true;
isEmpty(_)		->	false.

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


toList([X])	->	[X];
toList([X|Xs])	->	[X|Xs];
toList([])	->	[];
toList(E)	->	[E].



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

%% ====================================================================
%% map functions
%% ====================================================================

map(_, [])     -> [];
map(F,  [H|T]) -> [F(H)|map(F, T)].

%% ====================================================================
%% member functions
%% ====================================================================


uniqueList([X|Xs])	->
	case notMember(X,Xs) of
		true	->	[X|uniqueList(Xs)];
		false	->	uniqueList(Xs)
	end;
uniqueList([])	->	[].


notMember(X,[X|_])	->
	false;
notMember(X,[_|Ys])	->
	notMember(X,Ys);
notMember(_,[])		->
	true.

member2({I,N,_},[{I,N,_}|_])	-> true;
member2({I,N,D},[_|T])		-> member2({I,N,D},T);
member2({_,_,_},[])		-> false.

member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_, [])    -> false.



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


different(V,V1)	->
	case 	isElem(V)	and	isElem(V1)	of
	true	->
			case	 V == V1	of
			true		->	false;
			false	->	true
			end;
	false	->	true	
	end.

