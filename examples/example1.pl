%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

%:- style_check(all).

:- set_options(default),set_inference(backward(lw)).

nballs ~ uniform([1,2,3,4,5,6,7,8,9,10]) := true.

n2 ~ finite([0.9:1,0.05:5,0.05:6]) := true.

n3 ~ poisson(4) := true.

n4 ~ uniform([2,N]) := n2~=N.

ball(X) := nballs ~= N, between(1,N,X).

color(X) ~ uniform([green,red,white]) := nballs ~= N, between(1,N,X).


n3 := n2 ~= 1, true(0.5).

n3_1 := n2 ~= 1, sample(contUniform(0,1),X), X<0.5.

%draw(1) := true.
%draw(2) := true.
%draw(3) := true.



drawn(Draw) ~ uniform(Balls) := nballs ~= N, findall(X,between(1,N,X),Balls).

g ~ gaussian(0,0.01) := true.
g2 ~ gaussian(X,0.01) := g ~=X.


test(N) :-
	init, % initialize the system
	query([],[],drawn(1) ~= 1,N,P1,_,_), % query: pos evidence, negative evidence, query, num samples, result, other param,other param.
	E1 is abs(P1-( 0.1*(1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	write('Absolute error drawn(1) ~= 1: '),writeln(E1),
	
	query([],[],drawn(1) ~= 2,N,P2,_,_),
	E2 is abs(P2-( 0.1*(1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	write('Absolute error drawn(1) ~= 2: '),writeln(E2),
	
	query([],[],drawn(1) ~= 3,N,P3,_,_),
	E3 is abs(P3-( 0.1*(1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	write('Absolute error drawn(1) ~= 3: '),writeln(E3),
	
	query([],[],g ~= A,A,N,P0,_,_), % average: pos evidence, negative evidence,query,value to average, num samples, result, other param,other param.
	E0 is abs(0.0-P0),
	write('Absolute error average g ~ Gaussian(0,0.1): '),writeln(E0).

testpoisson(N) :-
	init,
	test_query([],[],n3 ~= 4,1,N,P1).

test1(N) :-
	init,
	test_query([],[],drawn(1) ~= 1,1,N,P1),
	C1 is 0.1*(1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10),
	write('correct answer: '),writeln(C1),nl,
	test_query([],[],nballs ~= X,X,N,P2),
	write('correct answer: '),writeln(5.5),nl,
	test_query([],[],g ~= X,X,N,P3),
	write('correct answer: '),writeln(0).

test2(N) :-
	init,
	query([],[],g ~= A,A,N,P0,_,_),
	E0 is abs(0.0-P0),
	write('Absolute error average g ~ Gaussian(0,0.1): '),writeln(E0),
	
	query([],[],(g ~= A,A<0.0),N,P,_,_),
	E1 is abs(0.5-P),
	write('Absolute error g<0.0: '),writeln(E1),
	
	query([],[],(g ~= A,A>0,A<0.1),N,P2,_,_),
	E2 is abs(0.341345-P2),
	write('Absolute error 0.0<g<0.1: '),writeln(E2),
	
	query([(g ~= A,A>0)],[],(g ~= A,A<0.1),N,P3,_,_),
	E3 is abs(0.68269-P3),
	write('Absolute error g<0.1 | g>0.0: '),writeln(E3),
	
	query([],[],(g ~= 0.0),N,P4,_,_),
	E4 is abs(3.98942280401433-P4),
	write('Absolute error density g=0.0: '),writeln(E4),
	
	query([],[],(g2 ~= 0.0),N,P5,_,_),
	E5 is abs(2.82094791773878-P5),
	write('Absolute error density g2=0.0: '),writeln(E5),
	
	query([g~=0.1],[],(g2 ~= 0.1),N,P6,_,_),
	E6 is abs(3.98942280401433-P6),
	write('Absolute error density g2=0.1|g=0.1: '),writeln(E6).


	
:- initialization(time(test(10000))). % automatically executed
