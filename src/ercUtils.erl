%% @author ethancollopy
%% @doc @todo Add description to ercUtils.


-module(ercUtils).
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


%% ====================================================================
%% Internal functions
%% ====================================================================


