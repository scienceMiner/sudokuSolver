%% @author ethancollopy
%% @doc @todo Add description to nq.


-module(nq).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
%%-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


domain(0)	->	[];
domain(N)	->	[N|domain(N-1)].

length1([X|Xs])		->
	1 + length1(Xs);
length1([])		-> 0;
length1(_)		-> 1.

