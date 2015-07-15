%%% -*- Mode: Prolog; -*-

:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

builtin(true).
builtin(findall(_,_,_)).
builtin(length(_,_)).
builtin(member(_,_)).
builtin(timestep(_)).
builtin(A=B).
builtin(A\==B).
builtin(A\=B).
builtin(A is B).
builtin(A > B).
builtin(A < B).
builtin(A >= B).
builtin(A =< B).
builtin(between(_,_,_)).
builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(sum_list(_,_)).
builtin(min(_,_)).
builtin(nth1(_,_,_)).
builtin(varia(_,_,_)).
builtin(product(_,_)).
builtin(densityGaussian(_,_,_,_)).
builtin(checkvalue(_,_)).
builtin(ground(_)).
builtin(trace).

satisfiable(ground(_)).
satisfiable(A=B).
satisfiable(A\==B).
satisfiable(A\=B).
satisfiable(A > B).
satisfiable(A < B).
satisfiable(A >= B).
satisfiable(A =< B).
satisfiable(findall(_,_,_)).

checkvalue(true,1.0).
checkvalue(false,0.0).

builtin(\+A) :-
	builtin(A).
builtin(write(_)).

:- set_options(default).
:- set_inference(false).

maze(X,Y):t ~ finite([0.8:free,0.2:pit]) := between(-3,3,X),between(-3,3,Y),ground(X),ground(Y),X>=0,Y>=0,(X,Y)\=(0,0).
maze(X,Y):t ~ finite([1:wall]) := between(-3,3,X),between(-3,3,Y),ground(X),ground(Y),X<0.
maze(X,Y):t ~ finite([1:wall]) := between(-3,3,X),between(-3,3,Y),ground(X),ground(Y),Y<0.

maze(0,0):t ~ uniform([free]) := true.

%wumpus:t ~ uniform([(-3,-3),(-3,-2),(-3,-1),(-3,0),(-3,1),(-3,2),(-3,3),(-2,-3),(-2,-2),(-2,-1),(-2,0),(-2,1),(-2,2),(-2,3),(-1,-3),(-1,-2),(-1,-1),(-1,0),(-1,1),(-1,2),(-1,3),(0,-3),(0,-2),(0,-1),(0,0),(0,1),(0,2),(0,3),(1,-3),(1,-2),(1,-1),(1,0),(1,1),(1,2),(1,3),(2,-3),(2,-2),(2,-1),(2,0),(2,1),(2,2),(2,3),(3,-3),(3,-2),(3,-1),(3,0),(3,1),(3,2),(3,3)]) := true.%findall((X,Y),(between(-3,3,X),between(-3,3,Y)),D).

gold(X,Y):t ~ finite([0.1:gold,0.9:null]) := true.

position:0 ~ finite([1:(0,0)]) := true.

%energy:0 ~ finite([1:1]) := true.


%energy:t+1 ~ gaussian(NewVal,0.0001) :=
%	energy:t ~= Val,
%	NewVal is Val*0.99.
gold(X,Y):t+1 ~ finite([1:Val]) := 
	gold(X,Y):t ~= Val.
	
maze(X,Y):t+1 ~ finite([1:Val]) := 
	maze(X,Y):t ~= Val.%,X=<2.
	
%maze(X,Y):t+1 ~ finite([1:Val]) := 
%	maze(X,Y):t ~= Val,X>2.
/*
maze(X,Y):t+1 ~ finite([0.999:free,0.001:wall]) := 
	maze(X,Y):t ~= free.
maze(X,Y):t+1 ~ finite([0.999:wall,0.001:free]) := 
	maze(X,Y):t ~= wall.
*/	
/*
change:t ~ finite([0.99:false,0.01:true]) := true.
wumpus:t+1 ~ finite([1:Val]) :=
	change:t ~= false,
	wumpus:t ~= Val.
	
wumpus:t+1 ~ uniform(D) := 
	change:t ~= true,
	findall((X,Y),(between(-2,2,X),between(-2,2,Y)),D).	
*/

wumpus:t+1 ~ finite([1:Val]) := 
	wumpus:t ~= Val.
	
/*
gold(X,Y):t+1 ~ finite([1:Val]) := 
	gold(X,Y):t ~= Val.

gold(X,Y):t+1 ~ finite([1:gold,0.000:null]) := 
	gold(X,Y):t ~= gold.
gold(X,Y):t+1 ~ finite([1:null,0.000:gold]) := 
	gold(X,Y):t ~= null.
*/
position:t+1 ~ finite([0.0:(Xold,Yold),1:(X,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(left),
	X is Xold - 1,
	\+(maze(X,Yold):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(left),
	X is Xold - 1,
	maze(X,Yold):t ~= wall.

position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(null).

position:t+1 ~ finite([0.0:(Xold,Yold),1:(X,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(right),
	X is Xold + 1,
	\+(maze(X,Yold):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(right),
	X is Xold + 1,
	maze(X,Yold):t ~= wall.


position:t+1 ~ finite([0.0:(Xold,Yold),1:(Xold,Y)]) :=
	position:t ~= (Xold,Yold),
	action(up),
	Y is Yold + 1,
	\+(maze(Xold,Y):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(up),
	Y is Yold + 1,
	maze(Xold,Y):t ~= wall.

position:t+1 ~ finite([0.0:(Xold,Yold),1:(Xold,Y)]) :=
	position:t ~= (Xold,Yold),
	action(down),
	Y is Yold - 1,
	\+(maze(Xold,Y):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(down),
	Y is Yold - 1,
	maze(Xold,Y):t ~= wall.
	
observation(up):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	maze(X,Ynext):t+1 ~= free.

observation(up):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	maze(X,Ynext):t+1 ~= wall.

observation(down):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	maze(X,Ynext):t+1 ~= free.

observation(down):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	maze(X,Ynext):t+1 ~= wall.
	
observation(left):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	maze(Xnext,Y):t+1 ~= free.

observation(left):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	maze(Xnext,Y):t+1 ~= wall.
	
observation(right):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	maze(Xnext,Y):t+1 ~= free.

observation(right):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	maze(Xnext,Y):t+1 ~= wall.


observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	wumpus:t+1 ~= (Xnext,Y).
	
observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	wumpus:t+1 ~= (Xnext,Y).

observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	wumpus:t+1 ~= (X,Ynext).

observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	wumpus:t+1 ~= (X,Ynext).

observation(stench):t+1 ~ finite([0.1:true,0.9:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	Ynext is Y + 1,
	Xprec is X - 1,
	Yprec is Y - 1,
	\+(wumpus:t+1 ~= (Xnext,Y)),
	\+(wumpus:t+1 ~= (X,Ynext)),
	\+(wumpus:t+1 ~= (Xprec,Y)),
	\+(wumpus:t+1 ~= (X,Yprec)).
	
observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	maze(Xnext,Y):t+1 ~= pit.
	
observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	maze(Xnext,Y):t+1 ~= pit.

observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	maze(X,Ynext):t+1 ~= pit.

observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	maze(X,Ynext):t+1 ~= pit.

observation(breeze):t+1 ~ finite([1.0:false,0.0:true]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	Ynext is Y + 1,
	Xprec is X - 1,
	Yprec is Y - 1,
	\+(maze(Xnext,Y):t+1 ~= pit),
	\+(maze(X,Ynext):t+1 ~= pit),
	\+(maze(Xprec,Y):t+1 ~= pit),
	\+(maze(X,Yprec):t+1 ~= pit).


observation(gold):t+1 ~ finite([0.2:true,0.8:false]) :=
	position:t+1 ~= (X,Y),
	gold(X,Y):t+1 ~= null.

	
observation(gold):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	gold(X,Y):t+1 ~= gold.


/*
init_particle(30).
step_particle([action(null)],[observation(breeze) ~=false],30).
eval_query_particle(current(position)~=_,30,P).
(between(1,30,I),writeln(I),printp(I),fail;true).


init_particle(30).
dcpf:step_particle1([action(null)],[observation(breeze) ~=false],[],30,1).
(between(1,30,I),writeln(I),printp(I),fail;true).
eval_query_particle(next(position)~=_,30,P).
trace.
dcpf:resampling(30).

! (between(1,30,I),writeln(I),printkeyp(I),fail;true).
init_particle(1000).
step_particle([action(null)],[observation(up) ~= wall,observation(right) ~= free],1000,1).
eval_query_particle(current(maze(0,1))~=wall,1000,P).
% real value is A is 0.4*0.9/(0.4*0.9+0.6*0.1). % 0.857142857142857

init_particle(100).
step_particle([action(null)],[observation(up) ~= wall,observation(right) ~= free],100,1).
plotdata(1).


init_particle(1).
step_particle([action(null)],[observation(up) ~= wall,observation(right) ~= free],1,1).
plotdata(1).

*/

mazesizemin(-4).
mazesizemax(4).

plotdata(Pos) :-
	bb_get(dcpf:offset,Offset),
	I is Offset+Pos,
	mazesizemin(Min),
	mazesizemax(Max),
	Tot is Max-Min,
	(
		between(0,Tot,YY),
		(
			between(Min,Max,X),
			(
				Y is Max-YY,
				(
					distributionalclause:proof_query_backward(I,current(wumpus) ~= (X,Y)) ->
						write('WW')
					;
					(
						recorded(I,current(maze(X,Y)) ~= _,_)
						->
						(
								distributionalclause:proof_query_backward(I,current(maze(X,Y)) ~= V),
								V==free
								->
								(
									distributionalclause:proof_query_backward(I,current(position) ~= (X,Y)) ->
									(
										distributionalclause:proof_query_backward(I,current(gold(X,Y)) ~= gold) ->
										write('A$')
										;
										write('AA')
									)
									;
									(
										distributionalclause:proof_query_backward(I,current(gold(X,Y)) ~= gold) ->
										write('$$')
										;
										write('  ')
									)
								)
								;
								write('##')
						)
						;
						write('??')
					)
				)
			),
			fail;
			true
		),
		nl,
		fail;
		true
	).

plotlatex(Pos,Dim) :-
	bb_get(dcpf:offset,Offset),
	I is Offset+Pos,
	mazesizemin(Min),
	mazesizemax(Max),
	(
		between(Min,Max,Y),
		(
			between(Min,Max,X),
			(
				Cx is (X-Min)*Dim,
				Cy is (Y-Min)*Dim,
				Cxx is Cx+Dim,
				Cyy is Cy+Dim,
				Mx is (X-Min)*Dim+Dim/2,
				My is (Y-Min)*Dim+Dim/2,
				(
					recorded(I,current(wumpus) ~= (X,Y),_) ->
						write('\\node at ('),write((Mx,My)),write(') {W};'),nl
					;
					(
						recorded(I,current(maze(X,Y)) ~= V,_)
						->
						(
				
								V==free
								->
								(
									recorded(I,current(position) ~= (X,Y),_) ->
									(
										recorded(I,current(gold(X,Y)) ~= gold,_) ->
										write('◢$')
										;
										write('\\node at ('),write((Mx,My)),write(') {A};'),nl
									)
									;
									(
										recorded(I,current(gold(X,Y)) ~= gold,_) ->
										write('$$')
										;
										true%write('  ')
									)
								)
								;
								write('\\draw[fill=black] ('),write((Cx,Cy)),write(') rectangle ('),write((Cxx,Cyy)),write(');'),nl
						)
						;
						write('\\node at ('),write((Mx,My)),write(') {?};'),nl
					)
				)
			),
			fail;
			true
		),
		nl,
		fail;
		true
	).
		
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

unittest :-
	kl(20000,KL,Diff),
	writeln(KL),
	writeln(Diff),
	eval_query_particle2((X,Y),current(maze(X,Y)) ~=free,10000,L),
	writeln('correct [1.0:(0,0),1.0:(1,0),1.0:(0,1),0.13985:(1,1),0.6882:(2,0)]'),
	write(' Got: '),writeln(L).

kl(Particles,KL,Diff) :-
	init_particle(Particles),
	((step_particle([action(null)],[observation(breeze) ~=false],Particles),
	step_particle([action(up)],[observation(breeze) ~=true],Particles),
	step_particle([action(down)],[observation(breeze) ~=false],Particles),
	step_particle([action(right)],[observation(breeze) ~=true],Particles))),
	eval_query_particle((current(maze(0,2))~=pit,current(maze(1,1))~=pit,current(maze(2,0))~=pit),Particles,W1),
	eval_query_particle((current(maze(0,2))~=pit,current(maze(1,1))~=pit,current(maze(2,0))~=free),Particles,W2),
	eval_query_particle((current(maze(0,2))~=pit,current(maze(1,1))~=free,current(maze(2,0))~=pit),Particles,W3),
	eval_query_particle((current(maze(0,2))~=free,current(maze(1,1))~=pit,current(maze(2,0))~=pit),Particles,W4),
	eval_query_particle((current(maze(0,2))~=free,current(maze(1,1))~=pit,current(maze(2,0))~=free),Particles,W5),
	KL is 0.034482759*log(0.034482759/W1)+0.137931034*log(0.137931034/W2)+0.137931034*log(0.137931034/W3)+0.137931034*log(0.137931034/W4)+0.551724138*log(0.551724138/W5),%!,write(KL),nl,
	Diff is abs(0.034482759-W1)+abs(0.137931034-W2)+abs(0.137931034-W3)+abs(0.137931034-W4)+abs(0.551724138-W5),!. %,write(Diff),nl.


meanTVD(Particles,N,MKL,T) :-
	In is cputime,
	findall(Diff,(between(1,N,I),kl(Particles,KL,Diff)),L),sum_list(L,S),MKL is S/N,
	T is (cputime-In)/N.


e :-
	meanTVD(50,100,MKL1,T1),
	meanTVD(100,100,MKL2,T2),
	meanTVD(500,100,MKL3,T3),
	meanTVD(1000,100,MKL4,T4),
	meanTVD(2000,100,MKL5,T5),
	writeln('particles,CPF'),
	write('50,'),  write(MKL1),nl,
	write('100,'), write(MKL2),nl,
	write('500,'), write(MKL3),nl,
	write('1000,'),write(MKL4),nl,
	write('2000,'),write(MKL5),nl,
	writeln('time,CPF'),
	write(T1),write(','),write(MKL1),nl,
	write(T2),write(','),write(MKL2),nl,
	write(T3),write(','),write(MKL3),nl,
	write(T4),write(','),write(MKL4),nl,
	write(T5),write(','),write(MKL5),nl.

%meandiff(Particles,N,MKL) :-
%	findall(Diff,(between(1,N,I),kl(Particles,KL,Diff)),L),sum_list(L,S),MKL is S/N.