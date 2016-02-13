%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)),set_query_propagation(true),set_debug(true).
%:- set_debug(true).
%builtin(size(_)).
%builtin(neighbor(_,_,_,_,_)).
%builtin(adjacent(_,_,_,_)).
%builtin(cell(_,_)).
builtin(iter(_)).
:- initialization(test(1000)).
% a(1,2,3)=..[_|A],findall(C=B,(member(B,A),ground(B)),L).
% 28.10.15
% cellular automaton to design a cave
% from
% Constructive generation methods for dungeons and levels (DRAFT)
% Noor Shaker, Antonios Liapis, Julian Togelius, Ricardo Lopes, and Rafael Bidarra
%
% random initial state, then at each time, each cell deterministically evolves based on its own state and those of its neighbors
%
%
% their example parameters:
% - grid size 50,
% - r=0.5 prob that a cell is a rock initially,
% - at least T=5 rocky neighbors (of Moore with N=1) make rock,
% - run n=2 steps
%
% this version replaces findall with custom counting for the 1-Moore-neighborhood and sets initial values of aux by hand 
% 

% grid size
size(50) := true. 
cell(X,Y) := size(S),between(1,S,X),between(1,S,Y).
aux_cell(X,Y) := size(S), M is S+1, between(0,M,X),between(0,M,Y).

% initially sprinkle with rocks randomly, setting aux to rock except in corners
rock(T,X,Y) ~ finite([0.5:rock,0.5:free]) := T=0, cell(X,Y).
rock(T,X,Y) ~ val(rock) := T=0, X=0, size(S), M is S-1, between(2,M,Y).
rock(T,X,Y) ~ val(rock) := T=0, Y=0, size(S), M is S-1, between(2,M,X).
rock(T,X,Y) ~ val(rock) := T=0,size(S), M is S-1, between(2,M,Y), X is S+1.
rock(T,Y,X) ~ val(rock) := T=0,size(S), M is S-1, between(2,M,Y), X is S+1.
rock(T,X,Y) ~ val(free) := T=0,X=0,Y=0.
rock(T,X,Y) ~ val(free) := T=0,X=0,Y=1.
rock(T,X,Y) ~ val(free) := T=0,X=1,Y=0.
rock(T,X,Y) ~ val(free) := T=0,size(S),X=0,Y=S.
rock(T,X,Y) ~ val(free) := T=0,size(S),Y=0,X=S.
rock(T,X,Y) ~ val(free) := T=0,size(S),X=0,Y is S+1.
rock(T,X,Y) ~ val(free) := T=0,size(S),Y=0,X is S+1.
rock(T,X,Y) ~ val(free) := T=0,size(S),X=1,Y is S+1.
rock(T,X,Y) ~ val(free) := T=0,size(S),Y=1,X is S+1.
rock(T,X,Y) ~ val(free) := T=0,size(S),X=S,Y is S+1.
rock(T,X,Y) ~ val(free) := T=0,size(S),Y=S,X is S+1.
rock(T,X,Y) ~ val(free) := T=0,size(S),X is S+1,Y is S+1.

% at later time, determine rock/free based on previous state
rock(T,X,Y) ~ val(rock) := aux_cell(X,Y), T > 0, TT is T-1, enough_rocky_neighbors(TT,X,Y).
rock(T,X,Y) ~ val(free) := aux_cell(X,Y), T > 0, TT is T-1, \+enough_rocky_neighbors(TT,X,Y).

% check neighbors to determine next value
% specialized to this neighborhood and threshold to avoid findalls
enough_rocky_neighbors(T,X,Y) :=
	enough_rocks(9,0,0,X,Y,T).  % go over nbs 9,8,..,1

% we've seen 5 rock, rest of neighbors doesn't matter
enough_rocks(_,R,_,_,_,_) := R=5,true.
% check whether neighbor N is rock or not (if that can still change the outcome)
enough_rocks(N,R,F,X,Y,T) :=
	N > 0,  % not all neighbors seen yet
	R < 5,  % not enough rocks seen yet
	F < 5,  % if we have seen 5 or more of 9 free, we know we cannot have 5 or more rocks & can fail
	neighbor(N,X,Y,XX,YY),
	rock(T,XX,YY) ~=rock,
	NN is N-1,
	RR is R+1,
	enough_rocks(NN,RR,F,X,Y,T).
enough_rocks(N,R,F,X,Y,T) :=
	N > 0,  % not all neighbors seen yet
	R < 5,  % not enough rocks seen yet
	F < 5,  % if we have seen 5 or more of 9 free, we know we cannot have 5 or more rocks & can fail
	neighbor(N,X,Y,XX,YY),
	\+(rock(T,XX,YY) ~=rock),
	NN is N-1,
	FF is F+1,
	enough_rocks(NN,R,FF,X,Y,T).
	
% 1-Moore-neighborhood 
neighbor(1,X,Y,X,Y) :=true. % (including cell itself)
neighbor(2,X,Y,X,YY) :=
	YY is Y+1.
neighbor(3,X,Y,X,YY) :=
	YY is Y-1.
neighbor(4,X,Y,XX,Y) :=
	XX is X+1.
neighbor(5,X,Y,XX,Y) :=
	XX is X-1.
neighbor(6,X,Y,XX,YY) :=
	XX is X+1,
	YY is Y+1.
neighbor(7,X,Y,XX,YY) :=
	XX is X+1,
	YY is Y-1.
neighbor(8,X,Y,XX,YY) :=
	XX is X-1,
	YY is Y+1.
neighbor(9,X,Y,XX,YY) :=
	XX is X-1,
	YY is Y-1.

% give number of iterations to run
iter(2).

% for sampling, ask which cells contain rock at the end...
rock(X,Y) := iter(I),cell(X,Y),rock(I,X,Y)~= rock.

% ... constraining to cases where the bottom right is reachable via a rock-free road from the top left
%evidence(reachable(X,S,S)) :- iter(X),size(S).

reachable(T,X,Y) := X=1,Y=1,rock(T,1,1)~=free.
reachable(T,X,Y) := rock(T,X,Y)~=free, adjacent(X,Y,XX,YY), reachable(T,XX,YY).
adjacent(X,Y,XX,YY) :=
	XX=X,
	YY is Y+1,
	cell(X,YY).
adjacent(X,Y,XX,YY) :=
	XX=X,
	YY is Y-1,
	cell(X,YY).
adjacent(X,Y,XX,YY) :=
	YY=Y,
	XX is X+1,
	cell(XX,Y).
adjacent(X,Y,XX,YY) :=
	YY=Y,
	XX is X-1,
	cell(XX,Y).

% querying rather than sampling:

%query(reachable(X,S,S)) :- iter(X),size(S).


test(N) :-
	init,
%	trace,
%	distributionalclause:expandquery3([rock(2,1,1)~=free],_132556),
	eval_query([reachable(2,50,50)],[],[findall_forward(1,(between(1,50,X),between(1,50,Y),rock(2,X,Y)~=V,writeln((X,Y,V))),_)],N,P,A,B), % p(nation~=a|student_gpa~=3.9)
	write('probability: '),writeln((P,A,B)).
	
	
%write('Particle '),open('s.txt','write',S),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(I,rock(2,XX,Y)~=rock,_),write(S,rock(XX,Y)),write(S,'.'),nl(S)),_),close(S),nl,writeln('------------------------------------');
		