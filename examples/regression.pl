%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)),set_query_propagation(false),set_debug(false).
%:- set_debug(true).
builtin(x(_,_,_)).
:- initialization(test_regression_adapt).

y(T) ~ gaussian(M,0.002) :=
	findall_forward(V,(w(N)~=W,x(T,N,X), V is W*X),L),
	sum_list(L,M1),
	w0~=W0,
	M is M1+W0.

w(N) ~ gaussian(0,0.1) :=
	x(_,N,_).

w0 ~ gaussian(0,4).

%x(T,N) ~ gaussian(0,100).

class(T,Class) ~ sigmoid(M,Class) :=
	findall_forward(V,(w(N,Class)~=W,x(T,N,X), V is W*X),L),
	sum_list(L,M).

w(N,Class) ~ gaussian(0,0.1) :=
	x(_,N,_).


test_regression(N) :-
	init,
	W0 is (random-0.5)*4,
	W1 is random-0.5,
	W2 is random-0.5,
	W3 is random-0.5,
	forall(between(1,30,T),(X1 is random*5,X2 is random*5,X3 is random*5,assert(x(T,1,X1)),assert(x(T,2,X2)),assert(x(T,3,X3)))),
	findall(y(T)~=Y,(x(T,1,X1),x(T,2,X2),x(T,3,X3),Y is W0+W1*X1+W2*X2+W3*X3+(random-0.5)/500),[y(1)~=F1,y(2)~=F2,y(3)~=F3|YList]),
	writeln(YList),
	eval_query_backward_lw(YList,[],y(1)~=Y1,Y1,N,P1,_,_),
	eval_query_backward_lw(YList,[],y(2)~=Y2,Y2,N,P2,_,_),
	eval_query_backward_lw(YList,[],y(3)~=Y3,Y3,N,P3,_,_),
	writeln(y(F1,P1)),
	writeln(y(F2,P2)),
	writeln(y(F3,P3)),
	Error is sqrt((F1-P1)^2+(F2-P2)^2+(F3-P3)^2),
	writeln(e(Error)).

test_classification(N) :-
	init,
	loaddata('digits',List),
	eval_query_backward_lw(List,[],class(1,1)~=1,1,N,P,_,_), 
	writeln(P).
	
	
test_regression_adapt :-
	N=1,
	init,
	W0 is (random-0.5)*4,
	W1 is random-0.5,
	W2 is random-0.5,
	W3 is random-0.5,
	forall(between(1,30,T),(X1 is random*5,X2 is random*5,X3 is random*5,assert(x(T,1,X1)),assert(x(T,2,X2)),assert(x(T,3,X3)))),
	findall(y(T)~=Y,(x(T,1,X1),x(T,2,X2),x(T,3,X3),Y is W0+W1*X1+W2*X2+W3*X3+(random-0.5)/500),[y(1)~=F1,y(2)~=F2,y(3)~=F3|D]),
	writeln(D),
%	trace,
	eval_query_valuelist(D,[],(w0~=WW0,w(1)~=WW1,w(2)~=WW2,w(3)~=WW3),(WW0,WW1,WW2,WW3),1000,InitD,_),
	writeln(eval_query_valuelist(D,[],(w0~=WW0,w(1)~=WW1,w(2)~=WW2,w(3)~=WW3),(WW0,WW1,WW2,WW3),1000,InitD,_)),
	writeln(InitD),
	bb_put(dis,InitD),
	forall(between(1,1000,I),(
	AA is I mod 3,
	
	(AA==0 ->
	(
	eval_query_valuelist(D,[],(w0~=WW0,w(1)~=WW1,w(2)~=WW2,w(3)~=WW3),(WW0,WW1,WW2,WW3),N,[W:El],_),
	bb_get(dis,Distribution1),
	
	bb_put(dis,[W:El|Distribution1]);true
	)
	;
	(
		false ->
		(
		bb_get(dis,Distribution),
		
		distributionalclause:sample(propfinite(Distribution),(M1,M2,M3,M4)),
	
	%	writeln(Distribution),nl,
%		writeln(sampled(M1,M2,M3,M4)),
	
		distributionalclause:sample(gaussian([M1,M2,M3,M4],[0.001,0	 ,0		,0,
															0	,0.001,0		,0,
															0	,0	 ,0.001	,0,
															0	,0	 ,0		,0.001]),(MN0,MN1,MN2,MN3)),
		
	%	writeln(eval_query_backward_lw([w0~=MN0,w(1)~=MN1,w(2)~=MN2,w(3)~=MN3],[],D,1,20,Pnew,_,_)),
		eval_query_backward_lw([w0~=MN0,w(1)~=MN1,w(2)~=MN2,w(3)~=MN3],[],D,1,20,Pnew,_,_),
%		writeln(new(Pnew:(MN0,MN1,MN2,MN3))),nl,
		(Pnew>0.0000 ->
		(bb_put(dis,[Pnew:(MN0,MN1,MN2,MN3)|Distribution]))
		;true
		)
		)
		;
		(
		bb_get(dis,Distribution),
		
		distributionalclause:sample(propfinite(Distribution),(M1,_,_,_)),
		distributionalclause:sample(propfinite(Distribution),(_,M2,_,_)),
		distributionalclause:sample(propfinite(Distribution),(_,_,M3,_)),
		distributionalclause:sample(propfinite(Distribution),(_,_,_,M4)),
	%	writeln(Distribution),nl,
%		writeln(sampled(M1,M2,M3,M4)),
	
		distributionalclause:sample(gaussian([M1,M2,M3,M4],[0.001,0	   ,0		,0,
															0	 ,0.001,0		,0,
															0	 ,0	   ,0.001	,0,
															0	 ,0	   ,0		,0.001]),(MN0,MN1,MN2,MN3)),
		
	%	writeln(eval_query_backward_lw([w0~=MN0,w(1)~=MN1,w(2)~=MN2,w(3)~=MN3],[],D,1,20,Pnew,_,_)),
		eval_query_backward_lw([w0~=MN0,w(1)~=MN1,w(2)~=MN2,w(3)~=MN3],[],D,1,5,Pnew,_,_),
%		writeln(new(Pnew:(MN0,MN1,MN2,MN3))),nl,
		(Pnew>0.0000 ->
		(bb_put(dis,[Pnew:(MN0,MN1,MN2,MN3)|Distribution]))
		;true
		)
		)
	))
	)
	),
	writeln(real(W0,W1,W2,W3)),
	bb_get(dis,Distr),
	findmax(Max,Distr),
	writeln(Max).