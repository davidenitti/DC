%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).
builtin(geometric(V)).

builtin(prob(P)).
builtin(dim(_,_)).
builtin(color(_,_)).
builtin(type(_,_)).
builtin(deltaT(_)).
builtin(varQ(_)).
builtin(cov(_,_,_)).


deltaT(1).
varQ(0.0004).

maxV(100).
maxV(D,V) :- V is 100-1.

% http://webee.technion.ac.il/people/shimkin/Estimation09/ch8_target.pdf page 3
cov(2,[Cov11,Cov12,Cov21,Cov22],VarQ) :-
	deltaT(DeltaT),
	Cov11 is VarQ*(DeltaT^3)/3,
	Cov12 is VarQ*(DeltaT^2)/2,
	Cov21 is VarQ*(DeltaT^2)/2,
	Cov22 is VarQ*(DeltaT).

cov(1,[Cov],VarQ) :-
	deltaT(DeltaT),
	Cov is VarQ*DeltaT^2.

% church: (hist (repeat 1000 flip) "Flips")
coin ~ finite([0.5:true,0.5:false]).

% N is the number of samples
test_coin(N) :-
	init,
	eval_query([],[],coin ~= true,N,P,_,_), % compute probability coin=true
	write('probability coin=true: '),writeln(P).

% alternative (non memoized):
test_coin2(N) :-
	init,
	eval_query([],[],true(0.5),N,P,_,_), % compute probability fair coin is true
	write('probability fair coin=true: '),writeln(P).
	

% product gaussians in Church: (* (gaussian 0 1) (gaussian 0 1) )
g ~ gaussian(0,1).
g2 ~ gaussian(0,1).
prod ~ val(P) :=
	g~=Val1,
	g2~=Val2,
	P is Val1*Val2.

test :-
	generate_backward([current(object(1))~=(0,0,0),action(move(1,1,0))],next(object(1)) ~= V,L),
	writeln(L).
	
	

object(ID):t+1 ~ indepGaussians([ ([NX],Cov), ([NY],Cov), ([0],Cov) ]) :=
	object(ID):t ~= (X,Y,Z),
%	\+stop:t,
	\+observation(object(ID)) ~=_,
	action(move(ID,DX,DY)),
	NX is X+DX,
	NY is Y+DY,
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).

object(ID):t+1 ~ indepGaussians([ ([X],Cov), ([Y],Cov), ([Z],Cov) ]) :=
	object(ID):t ~= (X,Y,Z),
	\+observation(object(ID)) ~=_,
	\+action(move(ID,_,_)),
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).