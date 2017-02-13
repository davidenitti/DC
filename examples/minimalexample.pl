%%% -*- Mode: Prolog; -*-

:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- set_options(default).





associate:t+1 ~ uniform(ObjectList1) :=
	observation(anchor(_))~=_,
	findall_forward(O, (current(object(A))~=O, \+observation(anchor(A))~=_), ObjectList1),
	writeln(ObjectList1).


object(Anchor):t+1 ~ val(Anchor) :=
	observation(anchor(Anchor)) ~= a,
	\+object(_):t ~=_.
object(Anchor):t+1 ~ val(Object) :=
	observation(anchor(Anchor)) ~= r,
	object(Anchor):t ~= Object.
object(A):t+1 ~ val(SampledObject) :=
	(current(object(A))~=_, \+observation(anchor(A))~=_, observation(anchor(_))~=_),
	associate:t+1 ~=SampledObject.


observation(anchor(_)):t+1 ~ val(_) :=
	true.

t1 :- %not okay
	N=1,
	init_particle(N),
	step_particle([],[observation(anchor(anchor1)) ~= a, observation(anchor(anchor2)) ~= a], N, 1),
	step_particle([],[observation(anchor(anchor1)) ~= r, observation(anchor(anchor3)) ~= a], N, 1),
	plotdata(N).

t2 :-%behavior okay
	N=1,
	init_particle(N),
	step_particle([],[observation(anchor(anchor2)) ~= a], N, 1),
	step_particle([],[observation(anchor(anchor3)) ~= a], N, 1),
	plotdata(N).


t3 :-%behavior okay
	N=1,
	init_particle(N),
	step_particle([],[observation(anchor(anchor1)) ~= a, observation(anchor(anchor2)) ~= a], N, 1),
	step_particle([],[observation(anchor(anchor1)) ~= r], N, 1),
	plotdata(N).



search_query(I,Q) :-
	eraseall(tempparticle),
	abolish_all_tables,
	distributionalclause:proof_query_backward_lazy(I,tempparticle,Q).
	
plotdata(N) :-
	dcpf:bb_get(offset,Offset),
	open('data.txt','write',S),
	(
			between(1,N,Pos),
			I is Offset+Pos,
			search_query(I,current(object(Anchor)) ~= Object),
			write(S,Anchor),write(S,' '),
			write(S,Object),
			nl(S),
			fail;
			true
	),
	nl(S),
	close(S).
