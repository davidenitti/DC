%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)),set_query_propagation(true).
%:- set_debug(true).

:- initialization(test).

a(1) := true.
a(X) := b(Y).
b(2) := true.

e(1,2) ~ finite([0.5:true,0.5:false]).
e(1,6) ~ finite([0.5:true,0.5:false]).
e(2,3) ~ finite([0.5:true,0.5:false]).
e(3,4) ~ finite([0.5:true,0.5:false]).
e(6,5) ~ finite([0.5:true,0.5:false]).
p(X,Y) := e(X,Y) ~=true.
p(X,Y) := e(X,Z) ~=true,p(Z,Y).

edge(1,2) := true.% X=1,Y=2.
edge(2,3) := true.%X=2,Y=3.
edge(1,4) := true.%X=1,Y=4.
edge(4,5) := true.%X=4,Y=5.

%edge(X,Y) := something(X,Y).
path(X,Y) := edge(X,Y).
path(X,Y) := edge(X,Z),path(Z,Y).

% N is the number of samples

test :-
	init,
	
	distributionalclause:expandquery5([p(1,4)],Out),
	writeln(Out),
	halt,
	findall(OutX2,(distributionalclause:executeq(Out,OutX),Out\==OutX,distributionalclause:simplify_query(0,0,OutX,OutX2)),L),
	writeln(ok1),
	findall(O,(member(O,L),O\==[false]),L2),
	writeln(ok2),
	distributionalclause:removedup(L2,L3,L2),
	writeln(ok3),
	test_to_list(Tuple,L3),
	distributionalclause:simplify_query(0,0,[Tuple],Tuple2),
	writeln(ok4),
	writeln(Tuple2).
	/*
	distributionalclause:expandquery_step([path(1,5)],Out,_),
	distributionalclause:simplify_queryAND(1,1,Out,Out1,0,_),flatten(Out1,Out11),
	distributionalclause:expandquery_step(Out11,Out2,_),
	distributionalclause:simplify_queryAND(1,1,Out2,Out22,0,_),flatten(Out22,Out222),
	distributionalclause:expandquery_step(Out222,Out3,_),
	distributionalclause:simplify_queryAND(1,1,Out3,Out33,0,_),flatten(Out33,Out333),
	distributionalclause:simplify_queryAND(1,1,Out333,Out3333,0,_),flatten(Out3333,Out33333),
	distributionalclause:expandquery_step(Out33333,Out4,_),
	distributionalclause:expandquery_step(Out4,Out5,_),
	findall(OutX,(executeq(Out5,OutX),OutX\=false),L),
	numbervars(Out,1,_),
	numbervars(Out1,1,_),
	numbervars(Out2,1,_),
	numbervars(Out3,1,_),
	numbervars(Out4,1,_),
	numbervars(Out5,1,_),
	numbervars(Out6,1,_),
	numbervars(Out7,1,_),
	writeln(Out3),
	writeln(Out33333),
	writeln(Out4),
	writeln(Out5),
%	writeln(Out33),
	*/
