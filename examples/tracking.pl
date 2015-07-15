%%% -*- Mode: Prolog; -*-

:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

builtin(checkvalue(_,_)).
builtin(normalize(_,_)).
builtin(optimalproposal(_,_,_,_,_,_,_)).
builtin(logIndepOptimalProposals(_,_,_)).

checkvalue(true,1.0).
checkvalue(false,0.0).

:- set_options(default).


builtin(prob(P)).
builtin(dim(_,_)).
builtin(color(_,_)).
builtin(type(_,_)).
builtin(deltaT(_)).
builtin(varQ(_)).
builtin(cov(_,_,_)).


deltaT(0.2).
varQ(0.005).

% http://webee.technion.ac.il/people/shimkin/Estimation09/ch8_target.pdf page 3
cov(2,[Cov11,Cov12,Cov21,Cov22],VarQ) :-
	deltaT(DeltaT),
	Cov11 is VarQ*(DeltaT^3)/3,
	Cov12 is VarQ*(DeltaT^2)/2,
	Cov21 is VarQ*(DeltaT^2)/2,
	Cov22 is VarQ*(DeltaT).
/*
% http://webee.technion.ac.il/people/shimkin/Estimation09/ch8_target.pdf page 3
cov([Cov11,Cov12,Cov21,Cov22],VarQ) :-
	deltaT(DeltaT),
	Cov11 is VarQ*(DeltaT^3)/3,
	Cov12 is VarQ*(DeltaT^2)/2,
	Cov21 is VarQ*(DeltaT^2)/2,
	Cov22 is VarQ*(DeltaT).
*/
cov(1,[Cov,0,0,0.0000001],VarQ) :-
	deltaT(DeltaT),
	Cov is VarQ*DeltaT^2.
	
prob(P) :- X is random, X<P.

%dim(0,(2,2,0.01)).
dim(table,(11,11,2)).

dim(1,(0.02,0.02,0.02)).
dim(2,(0.20,0.255,0.155)).
dim(3,(0.05,0.09,0.05)).
dim(4,(0.055,0.19,0.145)).
dim(5,(0.08,0.08,0.05)).
dim(6,(0.03,0.03,0.03)).
dim(7,(1.1,0.55,0.4)).

type(1,cube).
color(1,green).
rgbcolor(1,(0.0,1.0,0.0)).

color(2,brown).
rgbcolor(2,(0.8,0.5,0.1)).
type(2,box).

color(3,blue).
rgbcolor(3,(0.0,0.0,1.0)).
type(3,cube).

color(4,fucsia).
rgbcolor(4,(1.0,0.0,1.0)).
type(4,box).

color(5,grey).
rgbcolor(5,(0.3,0.3,0.3)).
type(5,cylinder).


color(6,white).
gnuplotcolor(6,10).
type(6,cube).


color(7,white).
rgbcolor(7,(1.0,1.0,1.0)).
type(7,table).

% state transition model

object(ID):t+1 ~  indepGaussians([ ([X,0],Cov), ([Y,0],Cov), ([Z,0],Cov) ]) :=
	observation(object(ID)) ~=(X,Y,Z),
	\+(object(ID):t ~=_),
	varQ(VarQ),
	cov(1,Cov,VarQ).

	
object(ID):t+1 ~ indepGaussians([ ([NX,0],Cov), ([NY,0],Cov), ([Z,0],Cov) ]) :=
	object(ID):t ~= (X,Vx,Y,Vy,Z,Vz),
	\+observation(object(ID)) ~=_,
	action(move(ID,DX,DY)),
	NX is X+DX,
	NY is Y+DY,
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).

object(ID):t+1 ~ indepGaussians([ ([X,0],Cov), ([Y,0],Cov), ([Z,0],Cov) ]) :=
	object(ID):t ~= (X,Vx,Y,Vy,Z,Vz),
	\+observation(object(ID)) ~=_,
	\+action(move(ID,_,_)),
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).
	
	
% observation model	
object(ID):t+1 ~ val(V) :=
	objectProposal(ID):t+1 ~= [V|W].

objectProposal(ID):t+1 ~ logIndepOptimalProposals([
			([NX,0],Cov, [1,0],[0.001],[OX]),
			([NY,0],Cov, [1,0],[0.001],[OY]),
			([Z,0],Cov, [1,0],[0.001],[OZ])]) :=
	object(ID):t ~= (X,Vx,Y,Vy,Z,Vz),
	observation(object(ID)) ~=(OX,OY,OZ),
	action(move(ID,DX,DY)),
	NX is X+DX,
	NY is Y+DY,
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).

objectProposal(ID):t+1 ~ logIndepOptimalProposals([
			([X,0],Cov, [1,0],[0.001],[OX]),
			([Y,0],Cov, [1,0],[0.001],[OY]),
			([Z,0],Cov, [1,0],[0.001],[OZ])]) :=
	object(ID):t ~= (X,Vx,Y,Vy,Z,Vz),
	observation(object(ID)) ~=(OX,OY,OZ),
	\+action(move(ID,_,_)),
	varQ(VarQ),
	cov(1,Cov,VarQ),
	deltaT(DeltaT).
	
observation(object(ID)):t+1 ~ logfinite([W:_]) :=
	objectProposal(ID):t+1 ~= [V|W].

observation(object(ID)):t+1 ~ finite([1:_]) :=
	\+(object(ID):t ~=_).
	
query_particle(A,B,C) :- eval_query_particle(A,B,C),write(C),nl.

q(1,A,B,C) :- eval_query_particle(A,B,C).
q(2,A,B,C) :- eval_query_particle_alternative(A,B,C).
q(3,A,B,C) :- eval_query_particle_alternative2(A,B,C).


/*
example:
init_particle(1000).
dcpf:step_particle_aux([],[observation(object(1)) ~= (0.6,1,0)],[],1000,0.200000),plotdata(1000).
dcpf:step_particle_aux([action(move(1,0.1,0.1))],[],[],1000,0.200000),plotdata(1000).
dcpf:step_particle_aux([],[observation(object(2)) ~= (0.1,0.2,0)],[],1000,0.200000),plotdata(1000).
dcpf:step_particle_aux([action(move(2,0.1,0.1))],[],[],1000,0.200000),plotdata(1000).




reward(ID):t+1 ~ val(R) :=
	object(ID):t+1 ~= (X,Vx,Y,Vy,Z,Vz),
	densityGaussian([0.6,1.0],[0.000001,0,0,0.000001],[X,Y],W),
	densityGaussian([0.6,1.0],[0.000001,0,0,0.000001],[0.6,1.0],Max),
	R is min(1,W/Max*2)-1.0.

cumulreward(ID):t+1 ~ val(Old) :=
	cumulreward(ID):t ~= Old,
	\+(reward(ID):t+1 ~= R).
	
cumulreward(ID):t+1 ~ val(TR) :=
	cumulreward(ID):t ~= Old,
	reward(ID):t+1 ~= R,
	TR is Old+R.
stop:t :=
	object(ID):t ~= (X,Vx,Y,Vy,Z,Vz),
	sqrt((X-0.6)^2+(Y-1)^2)<0.05.

policy(ID):t+1 ~ indepGaussians([ ([0],[0.005]), ([0],[0.005]) ]) :=
	\+action(move(DX,DY)),
	object(ID):t ~= _.
	
policy(ID):t+1 ~ val((DX,DY)) :=
	action(move(DX,DY)),
	object(ID):t ~= _.

cumulreward(ID):t ~ val(0) :=
	object(ID):t ~= _,
	action(move(DX,DY)).
	
t(A,B,Avg,Depth,Obs,N) :-
	init_particle(N),
	dcpf:step_particle_aux([],Obs,[],N,0.100000),plotdata(N),
	dcpf:step_particle_aux([action(move(A,B))],[],[],N,0.100000),%plotdata(N),
%	bb_put(sum,0.0),
	(
		between(1,Depth,I),
		dcpf:step_particle_aux([],[],[],N,0.100000),%plotdata(N),
%		dcpf:eval_average_particle([Avg:_],1,current(reward(1)),N,_),
%		bb_get(sum,S),
%		Ns is S+Avg,
%		bb_put(sum,Ns),
		fail;
		true
	),
	dcpf:eval_average_particle([TOT:_],1,current(cumulreward(1)),N,_),
	Avg is TOT.%/(Depth+1).

% Trials=number of first actions to sample
bestaction(Trials,Depth,Obs,(AA,BB),N) :-
	bb_put(maxv,(0,0)),
	bb_put(max,-10000000),
	(
		between(1,Trials,I),
		dcpf:sample(gaussian([0,0],[0.004,0,0,0.004]),(A,B)), % sample the first action
		t(A,B,Avg,Depth,Obs,N), % compute the average reward with the default policy
		writeln((A,B,Avg)),
		bb_get(max,M),
		(Avg>M ->
			(bb_put(maxv,(A,B)),bb_put(max,Avg))
			;
			true
		),
		fail;
		true
	),
	% null action
	t(0,0,Avg,Depth,Obs,N),
	writeln((0,0,Avg)),
	bb_get(max,M),
	(Avg>=M ->
		(bb_put(maxv,(0,0)),bb_put(max,Avg))
		;
		true
	),
	bb_get(max,MM),
	bb_get(maxv,(AA,BB)),
	writeln((best,AA,BB)).

plan(X,Y,0) :-!.

plan(X,Y,S) :-
	S>0,
	time(bestaction(20,25,[observation(object(1)) ~= (X,Y,0)],(AA,BB),200)),
	NX is X+AA,
	NY is Y+BB,
	writeln((plan,NX,NY)),
	NS is S-1,
	plan(NX,NY,NS).

*/

/*
bestaction(10,10,[observation(object(1)) ~= (0.5,0.5,0)],(AA,BB),500),NX is 0.5+AA,NY is 0.5+BB.





dcpf:step_particle_aux([action(move(A,B))],[],[],700,0.100000),plotdata(N),

plan(0.4,0.4,20).

init_particle(700).
dcpf:step_particle_aux([action(move(0,0))],[observation(object(1)) ~= (0.5,0.5,0.37)],[],700,0.100000),plotdata(700).
dcpf:step_particle_aux([],[],[],700,0.100000),plotdata(700).
dcpf:eval_average_particle(Avg1,1,current(reward(1)),700,P).
dcpf:step_particle_aux([],[],[],700,0.100000),plotdata(700).
dcpf:eval_average_particle(Avg2,1,current(reward(1)),700,P).
dcpf:step_particle_aux([],[],[],700,0.100000),plotdata(700).
dcpf:eval_average_particle(Avg3,1,current(reward(1)),700,P).





init_particle(700).
dcpf:step_particle_aux([action(move(0,0))],[observation(object(1)) ~= (0.5,0.0,0.37)],[],700,0.400000),plotdata(700).
dcpf:step_particle_aux([action(move(0,0))],[observation(object(1)) ~= (0.1,0.0,0.37)],[],700,0.400000),plotdata(700).
dcpf:eval_average_particle(Avg,1,current(reward(1),700,P).

q(1,current(reward(1)~=),B,C)
*/


		
averageobject(Particles,Mean) :-
	dcpf:bb_get(offset,Offset),
	bb_put(sumobj,0.0),
	(
		between(1,Particles,Pos),
		I is Offset+Pos,
		recorded(I,current(object) ~= Val,_),
		bb_get(sumobj,OldTOT),
		NewTOT is OldTOT+Val,
		bb_put(sumobj,NewTOT),
		fail;
		true
	),
	bb_delete(sumobj,T),
	Mean is T/Particles.
	
search_query(I,Q) :-
	distributionalclause:proof_query_backward(I,Q).
	
plotdata(N) :-
	%system("killall -q gnuplot"),
	dcpf:bb_get(offset,Offset),
	open('data.txt','write',S),
	%N2 is round(N/5-1),
	(
			between(1,N,Pos),
			I is Offset+Pos,
	%		recorded(I,current(position) ~= (RX,RY),_),
	%		write(S,RX),write(S,' '),write(S,RY),write(S,' 0.7 '),write(S,0),nl(S),
			search_query(I,current(object(ID)) ~= (X,_,Y,_,Z,_)),
%			search_query(I,current(orientation(ID)) ~= (RR,PP,YY)),
			dim(ID,(DX,DY,DZ)),
			rgbcolor(ID,(Rc,Gc,Bc)),
			write(S,ID),write(S,' '),
			write(S,X),write(S,' '),write(S,Y),write(S,' '),write(S,Z),write(S,' '),
%			write(S,DX),write(S,' '),write(S,DY),write(S,' '),write(S,DZ),write(S,' '),
%			write(S,RR),write(S,' '),write(S,PP),write(S,' '),write(S,YY),write(S,' '),
			write(S,Rc),write(S,' '),write(S,Gc),write(S,' '),write(S,Bc),nl(S),
			fail;
			true
	),
	nl(S),
	close(S).%,
	%system("killall -q gnuplot"),
	%system("gnuplot -persist gnuplot.txt").

