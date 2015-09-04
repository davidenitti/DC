%%% -*- Mode: Prolog; -*-

/*

Copyright 2014, Davide Nitti <firstname dot lastname at gmail dot com>, KU Leuven. All rights reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/lgpl.html/>.

This program uses library Orocos-BFL downloaded from http://svn.mech.kuleuven.be/repos/orocos/trunk/bfl 29/04/2014 and patched with https://www.fmtc.be/bugzilla/orocos/attachment.cgi?id=233

Orocos-BFL license:
Copyright (C) 2002 Klaas Gadeyne <first dot last at gmail dot com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

:- module(distributionalclause,[experiment_naive/8,experiment_LW/8,experiment_LWnoquery/8,experiment_LW2/8,experiment_LW_eval/8,experiment_LW_evalnoquery/8,diffsquared_multidim/4,test_query/6,query/7,query/8,eval_query_backward_lw/8,avgvar/3,product_wlist/3,eval_query_distribution_eval/8,eval_query_valuelist/7,eval_query_distribution/8,generate_forward/1,generate_backward/2,true/1,set_lifted/1,lifted/1,raoblackwellisation/1,set_options/1,set_current2nextcopy/1,log_likelihood_weighting/3,prod_scalar_multidim/3,eval_query_backward_exp/7,eval_query_backward_eval/8,eval_query_backward/7,matrixproduct/4,logIndepOptimalProposals/3,indepOptimalProposals/3,optimalproposal/7,set_debug/1,test_to_list/2,proof_query_backward_exp_eval/5,normalize/2,sum_list_multidim/3,divide_multidim/3,query_proof_defineRaoBackward/2,kalmanrao/14,kalmanrao_simplified/9,findmax/2,cleanDistribution/3,product_list/2,query_proof_setRaoBackward/5,sum_distrib/4,multiplyby/3,query_proof_rao/3,proof_query_backward_eval/4,proof_query_backward/3,proof_query_backward/2,timesyntax/2,likelihood_weighting/3,init/0,remove_builtin/2,prova/0,init_query_list/2,get_max_priority/1,derived/1,findManage/2,magic/0,init_query/2,eval_query_step/9,eval_distribution/7,magic_distributionalclause/4,magic_hardclause/0,magic_hardclause/3,magic_set_hard/3,magic_distributionalclause/0,magic_distributionalclause/3,evidence_proof_exists_maybe/1,sample/2,print_all/0,proof_exists_maybe/2,remove_inconsistent_value/5,check_value/2,clean_sample/1,sum_prob/2,divideby/3,normalize/2,assert_evidence/2,sample_lookahead/4, check_evidence/3,cumul/4,genesamplestep/4,generate_sample_pr/3,generate_sample/2,sample/2,query_proof/2,set_inference/1,inference/1,montecarlo/3,montecarlo/4,eval_query/8,eval_query/7,findall_forward/3,proof_query_backward_lazy/2,proof_query_backward_lazy/3,proof_query_backward_lazy_eval/3,proof_query_backward_lazy_eval/4]).

:- use_module('random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(library(terms)).

:- dynamic inference/1.
:- dynamic user:timestep/1.
:- dynamic user:deltaT/1.
:- dynamic user:evidence/2.
:- discontiguous user:(~)/2.
:- discontiguous user:(:=)/2.
:- dynamic user:(~)/2.
:- dynamic user:(~=)/2.
:- dynamic user:(:=)/2.
:- dynamic user:(pr)/2.
:- dynamic user:distributionalclause/4.
:- dynamic user:hardclause/3.
:- multifile user:builtin/1.
:- dynamic user:builtin/1.
:- dynamic user:adapt/1.
%:- dynamic action/1.
%:- style_check(all).

:- op(690,xfx,user:'~').
:- op(681,xfx,user:'~=').
:- op(1100,xfx,user:':=').
:- op(1101,xfx,user:'pr').

%abolish_all_tables :-!.
%:- yap_flag(tabling_mode,local).

% tabling unstable!

/*
:- table	tabling_proof_query_backward/2, tabling_proof_query_backward/3,
		tabling_proof_query_backward2/2, tabling_proof_query_backward2/3,
		tabling_proof_query_backward_evidence/2,tabling_proof_query_backward_evidence/3,
		tabling_proof_query_backward_lw/3,tabling_proof_query_backward_lw/4,
		tabling_proof_query_backward_lw2/5,tabling_proof_query_backward_lw2/6.
*/
/*
inittabling :- 
	table	tabling_proof_query_backward/2, tabling_proof_query_backward/3,
		tabling_proof_query_backward2/2, tabling_proof_query_backward2/3,
		tabling_proof_query_backward_evidence/2,tabling_proof_query_backward_evidence/3,
		tabling_proof_query_backward_lw/3,tabling_proof_query_backward_lw/4,
		tabling_proof_query_backward_lw2/5,tabling_proof_query_backward_lw2/6.
*/
% Buildin predicates: DC uses prolog instead of seeking them in the samples
user:builtin(true) :- !.
user:builtin(false) :- !.
user:builtin(findall(_,_,_)) :- !.
user:builtin(length(_,_)) :- !.
user:builtin(member(_,_)) :- !.
user:builtin(timestep(_)) :- !.
user:builtin(A=B) :- !.
user:builtin(A=..B) :- !.
user:builtin(A==B) :- !.
user:builtin(A\==B) :- !.
user:builtin(A\=B) :- !.
user:builtin(A is B) :- !.
user:builtin(A > B) :- !.
user:builtin(A < B) :- !.
user:builtin(A >= B) :- !.
user:builtin(A =< B) :- !.
user:builtin(integer(_)) :- !.
user:builtin(between(_,_,_)) :- !.
user:builtin(min_list(_,_)) :- !.
user:builtin(max_list(_,_)) :- !.
user:builtin(sum_list(_,_)) :- !.
user:builtin(sum_prob(_,_)) :- !.
user:builtin(min(_,_)) :- !.
user:builtin(max(_,_)) :- !.
user:builtin(nth1(_,_,_)) :- !.
user:builtin(nth0(_,_,_)) :- !.
user:builtin(sign(_)) :- !.
user:builtin(densityGaussian(_,_,_,_)) :- !.
user:builtin(\+A) :-
	user:builtin(A),!.
user:builtin(write(_)) :- !.
user:builtin(writeln(_)) :- !.
user:builtin(sample(_,_)) :- !.
user:builtin(true(P)) :- !.
user:builtin(trace) :- !.
user:builtin(debug) :- !.
user:builtin(ground(_)) :- !.
user:builtin(belief(_)) :- !.
%user:builtin(action(_)) :- !.

user:belief(A) :- ground(A),!.

true(P) :- X is random, X<P. %true(P) :- sample(contUniform(0,1),X), X<P.

set_options(default) :-
	set_inference(backward(classic)),
	set_lifted(false),
	set_raoblackwellisation(false),
	set_debug(false),
	set_current2nextcopy(true).
		
set_inference(V) :-
	retractall(inference(_)),
	assert(inference(V)).

set_lifted(V) :-
	retractall(lifted(_)),
	assert(lifted(V)).

set_raoblackwellisation(V) :-
	retractall(raoblackwellisation(_)),
	assert(raoblackwellisation(V)).

set_current2nextcopy(V) :-
	retractall(current2nextcopy(_)),
	assert(current2nextcopy(V)).

ps :-
	findall(A,(recorded(sampled,A,_), write(A),nl),_).

/*
set_inference(false) :-
	retractall(inference(_)),
	assert(inference(false)).
*/
get_magic(V) :-
	inference(V).

set_debug(V) :-
	retractall(use_debug(_)),
	assert(use_debug(V)).
	
get_debug(V) :-
	use_debug(V).
% to substitute uniform((A,B):(C,D)) with uniform([(A,B),...,(C,D)])
user:term_expansion((H~uniform((A,B):(C,D)):=Body),(H~uniform(Distribution):=Body)) :-
	ground((A,B):(C,D)),
	findall((X,Y),(between(A,C,X),between(B,D,Y)),Distribution).

user:term_expansion((H~uniform(A:C) := Body),(H~uniform(Distribution) := Body)) :-
	ground(A:C),
	findall(X,between(A,C,X),Distribution).

user:term_expansion((H~val(V) := Body),(H~uniform([V]) := Body)).

user:term_expansion((H~val(V)),(H~uniform([V]) := true)).

user:term_expansion((H~bool(P) := Body),(H~finite([P:true,Q:false]) := Body)) :- Q is 1-P.

user:term_expansion((H~bool(P)),(H~finite([P:true,Q:false]) := true)) :- Q is 1-P.

user:term_expansion((H~uniform((A,B):(C,D)):=Body pr PR),(H~uniform(Distribution):=Body pr PR)) :-
	ground((A,B):(C,D)),
	findall((X,Y),(between(A,C,X),between(B,D,Y)),Distribution).

user:term_expansion((H~uniform(A:C) := Body pr PR),(H~uniform(Distribution) := Body pr PR)) :-
	ground(A:C),
	findall(X,between(A,C,X),Distribution).
	
/*
% if magic is off substitute H~D:=B with distributionalclause and H:=B with hardclause
user:term_expansion(H~D,distributionalclause(H,D,true,0)) :- inference(false).
user:term_expansion((H~D:=B),distributionalclause(H,D,B,0)) :- inference(false).
user:term_expansion((H:=B),hardclause(H,B,0)) :-
	inference(false),
	H\=_~_.

user:term_expansion((H~D pr X),distributionalclause(H,D,true,X)) :- inference(false).
user:term_expansion((H~D:=B pr X),distributionalclause(H,D,B,X)) :- inference(false).
user:term_expansion((H:=B pr X),hardclause(H,B,X)) :-
	inference(false),
	H\=_~_.
*/



%%% verify if a formula is proved


query_proof(Key,true) :-
	!.
query_proof(Key,(A,B)) :-
	!,
	query_proof(Key,A),
	query_proof(Key,B).

% negation, to check
query_proof(Key,\+A) :-
	(
		user:builtin(A)
		->
		(
			%A=findall_forward(X,Y,Z)
			%->
			%	\+findall(X,query_proof(Key,Y),Z);
			\+user:A%,
%			write('false '),write(A),nl
		)
		;
		(
			%trace,
			
			
			\+recorded(Key,A,_)
		)
		
	).

query_proof(Key,A) :-
	A\=(\+_),
	(
		user:builtin(A)
		->
		(
			A=findall_forward(X,Y,Z)
			->
				findall(X,query_proof(Key,Y),Z);
				user:A
		)
		;
		(
			
			recorded(Key,A,_)
		)
	).



proof_query_backward(Key,true) :-
	!.
	
proof_query_backward(Key,(A,B)) :-
	!,
	proof_query_backward(Key,A),
	proof_query_backward(Key,B).





proof_query_backward(Key,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward(Key,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward(Key,\+A) :-
	\+proof_query_backward(Key,A),!.	


	
proof_query_backward(Key,A~= Val) :-
	ground(A),
	recorded(Key,A ~= Var,_),
	!,
	Val=Var.

proof_query_backward(Key,A) :-
	ground(A),
	recorded(Key,A,_),
	!.
/*	
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val,
	!.
*/	
	
% TO CHECK
proof_query_backward(Key,findall_forward(X,Y,Z)) :-
	(proof_query_backward(Key,Y),fail;true), % temporal solution
	findall(X,proof_query_backward(Key,Y),Z),
	!.
	
proof_query_backward(Key,A) :-
	recorded(Key,A,_).

/*
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	(
	(\+erased(R),recorded(Key,H ~= V,_)) -> % TO TEST
		(
			V=S,
			writeln('warning '),
			writeln(recorded(Key,H ~= distribution(D),R)),
			writeln( proof_query_backward(Key,H ~= S)),
			dcpf:printkeyp(Key),nl,
			erase(R)
		)
		;
		(
		recorda(Key,H ~= Val,_),
		S=Val
		)
	
	).
*/	

%%% Tabling %%%
proof_query_backward(Key,Head ~= Val) :-
	tabling_proof_query_backward(Key,Head,Distribution),
%	user:distributionalclause(Head,Distribution,Body,_),
%	proof_query_backward(Key,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Key,Head ~= Var,_),
	Var=Val.

proof_query_backward(Key,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward(Key,Head),
%	user:hardclause(Head,Body,_),
%	proof_query_backward(Key,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

%proof_query_backward(Key,A) :-
%	recorded(Key,A,_).
proof_query_backward(Key,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward(Key,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Key,Head ~= Var,_),
	Var=Val.
	
tabling_proof_query_backward(Key,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
%	(ground(Head)-> \+recorded(Key,Head~=_,_);true),
 	proof_query_backward(Key,Body).

	
tabling_proof_query_backward(Key,Head) :-
	user:hardclause(Head,Body,_),
%	(ground(Head)-> \+recorded(Key,Head,_);true),
	proof_query_backward(Key,Body).

%%% New NOT TESTED %%%
/*
init_particle(3).
recorda(global,distributionalclause(current(maze(A,B)),finite([0.0:pit,0.6:free,0.4:wall]),((A,B)\=(0,0)),0),_).
spy distributionalclause:sample/2,
step_particle([action(left)],[observation(energy) ~= 0.918774012016998,observation(up) ~= wall,observation(right) ~= wall,observation(down) ~= wall,observation(left) ~= wall],3,1).


recorda(global,distributionalclause(current(wumpus),finite([0.0204081632653061:(-3,-3),0.0204081632653061:(-3,-2),0.0204081632653061:(-3,-1),0.0204081632653061:(-3,0),0.0204081632653061:(-3,1),0.0204081632653061:(-3,2),0.0204081632653061:(-3,3),0.0204081632653061:(-2,-3),0.0204081632653061:(-2,-2),0.0204081632653061:(-2,-1),0.0204081632653061:(-2,0),0.0204081632653061:(-2,1),0.0204081632653061:(-2,2),0.0204081632653061:(-2,3),0.0204081632653061:(-1,-3),0.0204081632653061:(-1,-2),0.0204081632653061:(-1,-1),0.0204081632653061:(-1,0),0.0204081632653061:(-1,1),0.0204081632653061:(-1,2),0.0204081632653061:(-1,3),0.0204081632653061:(0,-3),0.0204081632653061:(0,-2),0.0204081632653061:(0,-1),0.0204081632653061:(0,0),0.0204081632653061:(0,1),0.0204081632653061:(0,2),0.0204081632653061:(0,3),0.0204081632653061:(1,-3),0.0204081632653061:(1,-2),0.0204081632653061:(1,-1),0.0204081632653061:(1,0),0.0204081632653061:(1,1),0.0204081632653061:(1,2),0.0204081632653061:(1,3),0.0204081632653061:(2,-3),0.0204081632653061:(2,-2),0.0204081632653061:(2,-1),0.0204081632653061:(2,0),0.0204081632653061:(2,1),0.0204081632653061:(2,2),0.0204081632653061:(2,3),0.0204081632653061:(3,-3),0.0204081632653061:(3,-2),0.0204081632653061:(3,-1),0.0204081632653061:(3,0),0.0204081632653061:(3,1),0.0204081632653061:(3,2),0.0204081632653061:(3,3)]),true,0),_).

(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true).
(recorded(global,A,_),write(A),nl,fail;true).
distributionalclause:current2next(global).
(recorded(global,A,_),write(A),nl,fail;true).
*/

%tabling_proof_query_backward(Key,Head,Distribution) :-
%	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
%	proof_query_backward(Key,Body).




% start proof_query_backward_evidence

proof_query_backward_evidence(Key,true) :-
	!.
	
proof_query_backward_evidence(Key,(A,B)) :-
	!,
	proof_query_backward_evidence(Key,A),
	proof_query_backward_evidence(Key,B).

% to check!	
proof_query_backward_evidence(Key,findall_forward(X,Y,Z)) :-
	(proof_query_backward_evidence(Key,Y),fail;true), % temporal solution
	findall(X,proof_query_backward_evidence(Key,Y),Z),
	!.

proof_query_backward_evidence(Key,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_evidence(Key,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_evidence(Key,\+A) :-
	\+proof_query_backward_evidence(Key,A),!.	

proof_query_backward_evidence(Key,A) :-
	ground(A),
	recorded(Key,A,_),
	!.

proof_query_backward_evidence(Key,A) :-
	recorded(Key,A,_).

proof_query_backward_evidence(Key,Head ~= Val) :-
	tabling_proof_query_backward_evidence(Key,Head,Distribution),
	(bb_get(queryevidence,QE)->true;QE=1),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	(user:evidence(Head ~= Var,QE) ->
		(
			ground(Head ~= Var) ->
			(
				Var=Val,
				likelihood_weighting(Var,Distribution,W),
				bb_get(wevidence,Wold),
				Wnew is Wold*W,
				bb_put(wevidence,Wnew)
			)
			;
			(
				(user:adapt(Head) ->
					(
					writeln('not implemented'),
					halt
					)
					;
					(
						sample(Distribution,Var)
					)
				)
			)
		)
		;
		(
		%sample(Distribution,Var)
		user:adapt(Head) ->
			(
				writeln('not implemented'),
				halt,
				recorded(proposal,localproposal(Head,PropD),_),
				sample(PropD,Var),
				%recorda(Key,Head ~= Var,_),
				likelihood_weighting(Var,Distribution,WN),
				likelihood_weighting(Var,PropD,WD),
				W is WN/WD,
				bb_get(wevidence,Wold),
				Wnew is Wold*W,
				bb_put(wevidence,Wnew)
			)
			;
			(
			sample(Distribution,Var)
			%recorda(Key,Head ~= Var,_),
			%W=1.0
			)
		
		)
	),
	recorda(Key,Head ~= Var,_),
	Var=Val.

proof_query_backward_evidence(Key,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_evidence(Key,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

tabling_proof_query_backward_evidence(Key,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward_evidence(Key,Body).

	
tabling_proof_query_backward_evidence(Key,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_evidence(Key,Body).


proof_query_backward_evidence(Key,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(bb_get(queryevidence,QE)->true;QE=1),
 	proof_query_backward_evidence(Key,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	(user:evidence(Head ~= Var,QE) ->
		(
		ground(Head ~= Var) ->
			(
				Var=Val,
				likelihood_weighting(Var,Distribution,W),
				bb_get(wevidence,Wold),
				Wnew is Wold*W,
				bb_put(wevidence,Wnew)
			)
			;
			(
				(user:adapt(Head) ->
					(
					writeln('not implemented'),
					halt
					)
					;
					(
						sample(Distribution,Var)
					)
				)
			)
		)
		;
		sample(Distribution,Var)
	),
	recorda(Key,Head ~= Var,_),
	Var=Val.

% end proof_query_backward_evidence

% TOCHECK: "A is b" in the body will not work and finite(List) may remain ungrounded
computeDistribution(finite(List),[(Val,finite(List2))],finite(List3)) :-
	findall(P:V,(member(P2:V2,List2),V2=Val,member(P1:V,List),P is P1*P2),NewDist),
	compactDistribution(finite(NewDist),finite(List3)).

compactDistribution(finite(List),finite(NewList)) :-
	length(List,Length),
	bb_put(templist,[]),
	(
		between(1,Length,I),
		nth1(I,List,P:Elem),
		bb_get(templist,TempList),
		\+member(_:Elem,TempList),
		bb_put(tempelem,P:Elem),
		NextI is I+1,
		(
			between(NextI,Length,I2),
			nth1(I2,List,P2:Elem),
			bb_get(tempelem,PP:Elem),
			PNew is PP+P2,
			bb_put(tempelem,PNew:Elem),
			fail;
			true
		),
		bb_get(tempelem,PP:Elem),
		PP>0,
		bb_get(templist,LL),
		bb_put(templist,[PP:Elem|LL]),
		fail;
		true
	),
	bb_get(templist,NewList).
	



current2next(Key) :-
	(
		recorded(Key,distributionalclause(current(Head),_,_,_),Ref),
		erase(Ref),
		fail;
		true
	),
%	print_distributionalclause_global,
	(
		recorded(Key,distributionalclause(next(Head),D,B,P),Ref),
		erase(Ref),
		\+(notsatisfiable(B)),
		recorda(Key,distributionalclause(current(Head),D,B,P),_),
		fail;
		true
	).

% not working
compact_body(Body,Output) :-
	test_to_list(Body,BodyList),
	BodyList=[A],
	Output=Body.

compact_body((HBody,TBody),Output) :-
	test_to_list((HBody,TBody),BodyList),
	BodyList=[H|T],
	duplicate_term(H,H2),
	duplicate_term(T,T2),
	member(H2,T),
	H==H2,
	T==T2,
	compact_body(TBody,Output).

compact_body((HBody,TBody),(HBody,Output)) :-
	test_to_list((HBody,TBody),BodyList),
	BodyList=[H|T],
	\+member(H,T),
	compact_body(TBody,Output).
	
notsatisfiable((H,T)) :-
	notsatisfiable(T).
	
notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A>B),
	member(A=<B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A<B),
	member(A>=B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A>=B),
	member(A<B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A=<B),
	member(A>B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A>B),
	ground(B),
	(member(A=<C,T);member(A<C,T)),
	ground(C),
	B>=C.

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A<B),
	ground(B),
	(member(A>=C,T);member(A>C,T)),
	ground(C),
	B=<C.
	
proof_query_backward_clause(Key,true,[],[]) :-
	!.
	
proof_query_backward_clause(Key,(A,B),Body,ListDistr) :-
	!,
	proof_query_backward_clause(Key,A,BodyA,ListA),
	proof_query_backward_clause(Key,B,BodyB,ListB),
	append(ListA,ListB,ListDistr),
	append(BodyA,BodyB,Body).


proof_query_backward_clause(Key,A,[A],[]) :-
	user:satisfiable(A),
	\+ground(A),!.

% todo: add case when ground(Val)
proof_query_backward_clause(Key,Head ~= Val,B,[(Val,Distribution)|D]) :-
	\+ground(Val),
	recorded(Key,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_clause(Key,Body,B,D).

%proof_query_backward_clause(Key,A,[],[]) :-
%	proof_query_backward(Key,A).
	
proof_query_backward_clause(Key,\+A,[],[]) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_clause(Key,A,[],[]) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.	

% TO CHECK
proof_query_backward_clause(Key,\+A,B,D) :-
	\+proof_query_backward_clause(Key,A,B,D),!.	



%%
proof_query_backward_clause2(Key,true,1) :-
	!.
	
proof_query_backward_clause2(Key,(A,B),W) :-
	!,
	proof_query_backward_clause2(Key,A,W1),
	proof_query_backward_clause2(Key,B,W2),
	W is W1*W2.

proof_query_backward_clause2(Key,A,1) :-
	user:satisfiable(A),
	\+ground(A),!.

% todo: add case when ground(Val)
proof_query_backward_clause2(Key,Head ~= Val,W) :-
	recorded(Key,distributionalclause(Head,finite(Distribution),Body,_),_),
	member(W1:Val,Distribution),
 	proof_query_backward_clause2(Key,Body,W2),
	W is W1*W2.

proof_query_backward_clause2(Key,\+A,1) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_clause2(Key,A,1) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.	

% TO CHECK
proof_query_backward_clause2(Key,\+A,B,D) :-
	\+proof_query_backward_clause2(Key,A,B,D),!.	
%%% End NEW %%%


	
% with a temporary index to store sampled variables
% don't use Key=Temp!

proof_query_backward(Key,Key,_) :-
	!,
	writeln('error proof_query_backward: Key=Temp'),
	!.

proof_query_backward(Key,Temp,true) :-
	!.
	
proof_query_backward(Key,Temp,(A,B)) :-
	!,
	proof_query_backward(Key,Temp,A),
	proof_query_backward(Key,Temp,B).


% Really slow! sometimes does not find all solutions!
proof_query_backward(Key,Temp,findall_forward(X,Y,Z)) :-
	(proof_query_backward(Key,Temp,Y),fail;true), % temporal solution
	findall(X,proof_query_backward(Key,Temp,Y),Z),
	!.

proof_query_backward(Key,Temp,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward(Key,Temp,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward(Key,Temp,\+A) :-
	\+proof_query_backward(Key,Temp,A),!.

proof_query_backward(Key,Temp,A) :-
	ground(A),
	recorded(Key,A,_),
	!.

proof_query_backward(Key,Temp,A) :-
	ground(A),
	recorded(Temp,A,_),
	!.
/*	
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,Temp,H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	%\+recorded(Temp,H ~= _,_), % is always true
	sample(D,Val),
%	erase(R),
	recorda(Temp,H ~= Val,_),
	S=Val,
	!.
*/
%%% Tabling %%%

proof_query_backward(Key,Temp,A) :-
	recorded(Key,A,_).
	
proof_query_backward(Key,Temp,A) :-
	recorded(Temp,A,_).
/*
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,Temp,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	recorda(Temp,H ~= Val,_),
	S=Val.
*/

proof_query_backward(Key,Temp,Head ~= Val) :-
	tabling_proof_query_backward2(Key,Temp,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward(Key,Temp,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward(Key,Temp,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward(Key,Temp,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward2(Key,Temp,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).
	
tabling_proof_query_backward2(Key,Temp,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward(Key,Temp,Body).
	
tabling_proof_query_backward2(Key,Temp,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward(Key,Temp,Body).
	


% don't use Key=Temp
proof_query_backward_eval(Key,Temp,true,1.0) :-
	!.

% error in proof_query_backward_eval	
proof_query_backward_eval(Key,Temp,(A,B),W) :-
	!,
	proof_query_backward_eval(Key,Temp,A,W1),
	proof_query_backward_eval(Key,Temp,B,W2),
	W is W1*W2.

% TO CHECK
proof_query_backward_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
	(proof_query_backward(Key,Temp,Y),fail;true), % temporal solution
	findall(X,proof_query_backward(Key,Temp,Y),Z),
	!.

proof_query_backward_eval(Key,Temp,\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_eval(Key,Temp,A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_eval(Key,Temp,\+A,1.0) :-
	\+proof_query_backward(Key,Temp,A),!.


proof_query_backward_eval(Key,Temp,A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Key,A,_),
	!.

proof_query_backward_eval(Key,Temp,A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Temp,A,_),
	!.
/*	
% to support non-sampled variables H ~= distribution(D) in the particles	
proof_query_backward_eval(Key,Temp,H~=Val,W) :-
	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	likelihood_weighting(Val,D,W),
	recorda(Temp,H~=Val,_),
	!.
*/

/*
proof_query_backward_eval(Key,Temp,H~=Val,W) :-
	ground(H~=Val),
	recorded(Temp,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	likelihood_weighting(Val,D,W),
	recorda(Temp,H~=Val,_),
	!.
*/	
%%% Tabling %%%

proof_query_backward_eval(Key,Temp,A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_).
	
proof_query_backward_eval(Key,Temp,A,1.0) :-
	A\=(\+_),
	recorded(Temp,A,_).
/*
proof_query_backward_eval(Key,Temp,H ~= S,1.0) :-
	\+ground(S),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	recorda(Temp,H ~= Val,_),
	S=Val.
*/
	
proof_query_backward_eval(Key,Temp,Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward(Key,Temp,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Temp,Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward(Key,Temp,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Temp,Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward(Key,Temp,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).

/*
proof_query_backward_eval(Key,Temp,Head,0.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	\+proof_query_backward(Key,Temp,Body).	
*/


%proof_query_backward_eval(Key,Temp,A,0.0) :-
%	\+proof_query_backward_eval(Key,Temp,A,_).

% split list
splitlist([],[],[]) :- !.
splitlist([(L1,L2)],[L1],[L2]) :- !.
splitlist([(L1,L2)|T],[L1|T1],[L2|T2]) :- 
	splitlist(T,T1,T2).
	
checkdistribution(finite(L),Var) :-
	!,
	copy_term(Var,Var2),
	member(_:Var2,L),!.
checkdistribution(uniform(L),Var) :-
	!,
	copy_term(Var,Var2),
	member(Var2,L),!.
	
checkdistribution(_,Var) :- !.
% start check evidence eval for DC
proof_query_backward_eval(Key,true,1.0) :-
	!.

proof_query_backward_eval(Key,(A,B),W) :-
	!,
	proof_query_backward_eval(Key,A,W1),
	proof_query_backward_eval(Key,B,W2),
	W is W1*W2.

% CHECK the weight
proof_query_backward_eval(Key,findall_forward(X,Y,Z),1.0) :-
	(proof_query_backward_evidence(Key,Y),fail;true), % temporal solution
	findall(X,proof_query_backward_evidence(Key,Y),Z),
	!.

proof_query_backward_eval(Key,\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_eval(Key,A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_eval(Key,\+A,1.0) :-
	\+proof_query_backward_evidence(Key,A),!.

proof_query_backward_eval(Key,A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Key,A,_),
	!.
/*
% to support non-sampled variables H ~= distribution(D) in the particles		
proof_query_backward_eval(Key,H~=Val,W) :-
	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	likelihood_weighting(Val,D,W),
	erase(R),
	recorda(Key,H~=Val,_),
	!.
*/	
%%% Tabling %%%

proof_query_backward_eval(Key,A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_).
/*
proof_query_backward_eval(Key,H ~= S,1.0) :-
	\+ground(S),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val.
*/


proof_query_backward_eval(Key,Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
%	checkdistribution(Distribution,Var),
	(
	 	proof_query_backward_evidence(Key,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Key,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				(user:adapt(Head) ->
					(
					writeln('not implemented'),
					halt,
					recorded(proposal,localproposal(Head,PropD),_),
					sample(PropD,Var),
					recorda(Key,Head ~= Var,_),
					likelihood_weighting(Var,Distribution,WN),
					likelihood_weighting(Var,PropD,WD),
					W is WN/WD
					)
					;
					(
					sample(Distribution,Var),
					recorda(Key,Head ~= Var,_),
					W=1.0
					)
				)
			)
		)
	).

proof_query_backward_eval(Key,Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_evidence(Key,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Key,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Key,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_evidence(Key,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

% end check evidence eval

% start proof_query_backward_lw

proof_query_backward_lw(Key,true) :-
	!.
	
proof_query_backward_lw(Key,(A,B)) :-
	!,
	proof_query_backward_lw(Key,A),
	proof_query_backward_lw(Key,B).



proof_query_backward_lw(Key,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lw(Key,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_lw(Key,\+A) :-
	\+proof_query_backward_lw(Key,A),!.	


proof_query_backward_lw(Key,A~= Val) :-
	ground(A),
	recorded(Key,A ~= Var,_),
	!,
	Val=Var.


proof_query_backward_lw(Key,A) :-
	ground(A),
	recorded(Key,A,_),
	!.

% to check!	
proof_query_backward_lw(Key,findall_forward(X,Y,Z)) :-
	(proof_query_backward_lw(Key,Y),fail;true), % temporal solution
	findall(X,proof_query_backward_lw(Key,Y),Z),
	!.
	
proof_query_backward_lw(Key,A) :-
	recorded(Key,A,_).


proof_query_backward_lw(Key,Head ~= Val) :-
%	tabling_proof_query_backward_lw(Key,Head,Distribution,UnifBody),
	user:distributionalclause(Head,Distribution,UnifBody,_),
 	proof_query_backward_lw(Key,UnifBody),
%	test_to_list(Q,Qlist),
	ground(Head),
	ground(Distribution),
	
	\+recorded(Key,Head ~= _,_),
	bb_get(q,QQ), %writeln(QQ), % todo TO TEST!!!!
%	bb_get(nq,NQQ),
%	((between(1,NQQ,Pos),arg(Pos,QQ,Elem),unifiable(Head ~= _,Elem,_)) -> 
	%(nth0(Pos,QQ,Head ~= _) ->
%	writeln(QQ),
	(inlist(Head ~= _,QQ,Head2~=Var2,Rest) ->
	(
		%bb_get(q,QQ2),
		%arg(Pos,QQ2,Head2~=Var2),
		%nth0(Pos,QQ2,Head2~=Var2,Rest),
		%Elem= Head2~=Var2,
		(
		(ground(Head2~=Var2))-> % Head and value ground in the original query
		(
		%	writeln(InQ= Head ~= Val),
			Var=Val,
			Var2=Val,
			bb_put(q,Rest),
			likelihood_weighting(Val,Distribution,W),
			((Distribution=beta(_,_);Distribution=gaussian(_,_))-> ( bb_get(dx,OldDX),DX is OldDX+1, bb_put(dx,DX) );true), % to complete
			bb_get(wevidence,Wold),
			Wnew is Wold*W,
			bb_put(wevidence,Wnew),
			(Wnew>0 -> true; (!,fail))
			%bb_put(q,QQ2)%
			%,
			%writeln(lw(Wnew,QQ2,Rest))
		)
		;
			(
			sample(Distribution,Var),
			(
			(ground(Head2) )->
				(
					Head=Head2,Var=Var2,
					%writeln(newq(QQ))
					%setarg(Pos,QQ,null),
					bb_put(q,Rest)%,
					%writeln(newq(QQ))
				)
			;
				true
			)
			)
		)
	)
	;
		sample(Distribution,Var)
	),

	recorda(Key,Head ~= Var,_),
	Var=Val.

proof_query_backward_lw(Key,Head) :-
	Head\=(_ ~= _),
	%tabling_proof_query_backward_lw(Key,Head,_),
	user:hardclause(Head,Body,_),
	proof_query_backward_lw(Key,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).
/*
tabling_proof_query_backward_lw(Key,Head,Distribution,Body) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward_lw(Key,Body).

	
tabling_proof_query_backward_lw(Key,Head,Body) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_lw(Key,Body).
*/

proof_query_backward_lw(Key,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
%	writeln('totest! proof_query_backward_lw global'),
%	halt,
%	test_to_list(Q,Qlist),
 	proof_query_backward_lw(Key,Body),
	ground(Head),
	ground(Distribution),
	
	\+recorded(Key,Head ~= _,_),
	bb_get(q,QQ), %writeln(QQ), % todo TO TEST!!!!
%	bb_get(nq,NQQ),
	%((between(1,NQQ,Pos),arg(Pos,QQ,Elem),unifiable(Head ~= _,Elem,_)) -> %
	(inlist(Head ~= _,QQ,Head2~=Var2,Rest) -> %(nth0(Pos,QQ,Head ~= _) ->
	(
		%bb_get(q,QQ2),
		%arg(Pos,QQ2,Head2~=Var2),%
		%nth0(Pos,QQ2,Head2~=Var2,Rest),
		%Elem= Head2~=Var2,
		(
		(ground(Head2~=Var2))-> % Head and value ground in the original query
		(
		%	writeln(InQ= Head ~= Val),
			Var=Val,
			Var2=Val,
			bb_put(q,Rest),
			likelihood_weighting(Val,Distribution,W),
			((Distribution=beta(_,_);Distribution=gaussian(_,_))-> ( bb_get(dx,OldDX),DX is OldDX+1, bb_put(dx,DX) );true), % to complete
			bb_get(wevidence,Wold),
			Wnew is Wold*W,
			bb_put(wevidence,Wnew),
			(Wnew>0 -> true; (!,fail))
			%,
			%writeln(lw(Wnew,QQ2,Rest))
		)
		;
			(
			sample(Distribution,Var),
			(
			(ground(Head2) )->
				(
					Head=Head2,Var=Var2,
					%writeln(newq(QQ))
					%setarg(Pos,QQ,null),
					b_put(q,Rest)%,
					%writeln(newq(QQ))
				)
			;
				true
			)
			)
		)
	)
	;
		sample(Distribution,Var)
	),

	recorda(Key,Head ~= Var,_),
	Var=Val.

% end proof_query_backward_lw

inlist(Elem,[F|Rest2],UN,Rest3) :-
	(unifiable(F,Elem,_)->
	(
		UN=F,
		Rest3=Rest2
	)
	;
	(
		inlist(Elem,Rest2,UN,Rest),
		Rest3=[F|Rest]
	)
	).

%%%% start proof_query_backward_lw_adapt %%%

proof_query_backward_lw_adapt(Key,true,Q) :-
	!.
	
proof_query_backward_lw_adapt(Key,(A,B),Q) :-
	!,
	proof_query_backward_lw_adapt(Key,A,Q),
	proof_query_backward_lw_adapt(Key,B,Q).

% to check!	
proof_query_backward_lw_adapt(Key,findall_forward(X,Y,Z),Q) :-
	(proof_query_backward_lw_adapt(Key,Y,Q),fail;true), % temporal solution
	findall(X,proof_query_backward_lw_adapt(Key,Y,Q),Z),
	!.

proof_query_backward_lw_adapt(Key,\+A,Q) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lw_adapt(Key,A,Q) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_lw_adapt(Key,\+A,Q) :-
	\+proof_query_backward_lw_adapt(Key,A,Q),!.	


proof_query_backward_lw_adapt(Key,A,Q) :-
	ground(A),
	recorded(Key,A,_),
	!.

proof_query_backward_lw_adapt(Key,A,Q) :-
	recorded(Key,A,_).

proof_query_backward_lw_adapt(Key,Head ~= Val,Q) :-
	tabling_proof_query_backward_lw_adapt(Key,Head,Distribution,Q,UnifBody),
%	test_to_list(Q,Qlist),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	duplicate_term(Q,CopyQ),
	(nth0(Pos,CopyQ,Head ~= Var) ->
		(
			((nth0(Pos,Q,HeadQ ~= VarQ),ground(HeadQ~= VarQ),ground(Head ~= Var)) ->
			(
				Var=Val,
				likelihood_weighting(Var,Distribution,W),
				bb_get(wevidence,Wold),
				Wnew is Wold*W,
				bb_put(wevidence,Wnew),
				WD=1
			)
			;
			(
				(user:adapt(Head) ->
					(
					%trace,
					
					(recorded(proposal,localproposal(Head,Distribution,UnifBody,TypeD,PropD),_) ->
					true ; defaultproposal(Head,UnifBody,TypeD,Distribution,PropD)),
					(TypeD==finite ->
					(
					prod_distrib(Distribution,finite(PropD),ProdNorm) % product between the original distribution and the 
					
					)
					;
					writeln('not implemened')
					),
					
					sample(ProdNorm,Var),
					%recorda(Key,Head ~= Var,_),
					likelihood_weighting(Var,Distribution,WN), % original distribution
					likelihood_weighting(Var,ProdNorm,WD), % proposal distribution
									
					W is WN/WD,
					%todo compute derivative for finite
					likelihood_weighting(Var,finite(PropD),ParamF),
					RelDer is 1/ParamF,
					%writeln((W,RelDer)),
					recorda(proposal,relderivative(Key,Head,Distribution,UnifBody,Var,W),_), % for finite!
					bb_get(wevidence,Wold),
					Wnew is Wold*W,
					bb_put(wevidence,Wnew)
					)
					;
					(
						sample(Distribution,Var),
						likelihood_weighting(Var,Distribution,WD)
					)
				)
			)),
			%writeln(copyQ),
			%writeln(CopyQ),
			%writeln(q),
			%writeln(Q),
			((nth0(Pos,Q,HeadQ ~= VarQ),ground(HeadQ)) -> (Head=HeadQ,Var=VarQ);true) %to check
			%writeln(q),
			%writeln(Q)
		)
		;
		(user:adapt(Head) ->
					(
					%trace,
					
					(recorded(proposal,localproposal(Head,Distribution,UnifBody,TypeD,PropD),_) ->
					true ; defaultproposal(Head,UnifBody,TypeD,Distribution,PropD)),
					(TypeD==finite ->
					(
					prod_distrib(Distribution,finite(PropD),ProdNorm)
					
					)
					;
					writeln('not implemened')
					),
					
					sample(ProdNorm,Var),
					%recorda(Key,Head ~= Var,_),
					likelihood_weighting(Var,Distribution,WN),
					likelihood_weighting(Var,ProdNorm,WD),
									
					W is WN/WD,
					%todo compute derivative for finite
					likelihood_weighting(Var,finite(PropD),ParamF),
					RelDer is 1/ParamF,
					%writeln((W,RelDer)),
					recorda(proposal,relderivative(Key,Head,Distribution,UnifBody,Var,W),_), % for finite!
					bb_get(wevidence,Wold),
					Wnew is Wold*W,
					bb_put(wevidence,Wnew)
					)
					;
					(
						sample(Distribution,Var),
						likelihood_weighting(Var,Distribution,WD)
					)
				)
	),
	(
	user:adapt(_) ->
		(
		recorded(proposal,proposalprob(Key,PPold),Ref),
		erase(Ref),
		PPnew is PPold*WD,
		recorda(proposal,proposalprob(Key,PPnew),_)
		)
		;
		true
	),
	recorda(Key,Head ~= Var,_),
	Var=Val.

proof_query_backward_lw_adapt(Key,Head,Q) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_lw_adapt(Key,Head,Q,_),	
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

tabling_proof_query_backward_lw_adapt(Key,Head,Distribution,Q,Body) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward_lw_adapt(Key,Body,Q).

	
tabling_proof_query_backward_lw_adapt(Key,Head,Q,Body) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_lw_adapt(Key,Body,Q).


proof_query_backward_lw_adapt(Key,Head ~= Val,Q) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
%	test_to_list(Q,Qlist),
 	proof_query_backward_lw_adapt(Key,Body,Q),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	duplicate_term(Q,CopyQ),
	(nth0(Pos,CopyQ,Head ~= Var) ->
%	(member(Head ~= Var,Q) ->
		(
			(ground(Head ~= Var) ->
			(
				Var=Val,
				likelihood_weighting(Var,Distribution,W),
				bb_get(wevidence,Wold),
				Wnew is Wold*W,
				bb_put(wevidence,Wnew)
			)
			;
			(
				(user:adapt(Head) ->
					(
					writeln('not implemented'),
					halt
					)
					;
					(
						sample(Distribution,Var)
					)
				)
			)),
			%writeln(copyQ),
			%writeln(CopyQ),
			%writeln(q),
			%writeln(Q),
			((nth0(Pos,Q,HeadQ ~= VarQ),ground(HeadQ)) -> (Head=HeadQ,Var=VarQ);true)
			%writeln(q),
			%writeln(Q)
		)
		;
		sample(Distribution,Var)
	),
	recorda(Key,Head ~= Var,_),
	Var=Val.

%%%% end proof_query_backward_lw_adapt %%%%


% start proof_query_backward_lw temp: 
% TODO remove last argument
proof_query_backward_lw(Key,Key,_,_) :-
	!,
	writeln('error proof_query_backward: Key=Temp'),
	!.
	
proof_query_backward_lw(Key,Temp,true,_) :-
	!.
	
proof_query_backward_lw(Key,Temp,(A,B),_) :-
	!,
	proof_query_backward_lw(Key,Temp,A,_),
	proof_query_backward_lw(Key,Temp,B,_).

% to check!	
proof_query_backward_lw(Key,Temp,findall_forward(X,Y,Z),_) :-
	(proof_query_backward_lw(Key,Temp,Y,_),fail;true), % temporal solution
	findall(X,proof_query_backward_lw(Key,Temp,Y,_),Z),
	!.

proof_query_backward_lw(Key,Temp,\+A,_) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lw(Key,Temp,A,_) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_lw(Key,Temp,\+A,_) :-
	\+proof_query_backward_lw(Key,Temp,A,_),!.	

proof_query_backward_lw(Key,Temp,A~= Val,_) :-
	ground(A),
	recorded(Key,A ~= Var,_),
	!,
	Val=Var.

proof_query_backward_lw(Key,Temp,A~= Val,_) :-
	ground(A),
	recorded(Temp,A ~= Var,_),
	!,
	Val=Var.
		
proof_query_backward_lw(Key,Temp,A,_) :-
	ground(A),
	recorded(Key,A,_),
	!.
proof_query_backward_lw(Key,Temp,A,_) :-
	ground(A),
	recorded(Temp,A,_),
	!.
	
proof_query_backward_lw(Key,Temp,A,_) :-
	recorded(Key,A,_).
proof_query_backward_lw(Key,Temp,A,_) :-
	recorded(Temp,A,_).
	
proof_query_backward_lw(Key,Temp,Head ~= Val,_) :-
	tabling_proof_query_backward_lw2(Key,Temp,Head,Distribution,_,UnifBody),
%	test_to_list(Q,Qlist),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	bb_get(q,QQ),
	(nth0(Pos,QQ,Head ~= _) ->
	(
		bb_get(q,QQ2),
		nth0(Pos,QQ2,Head2~=Var2,Rest),
		(
		(ground(Head2~=Var2))-> % Head and value ground in the original query
		(
		%	writeln(InQ= Head ~= Val),
			Var=Val,
			Var2=Val,
			bb_put(q,Rest),
			likelihood_weighting(Val,Distribution,W),
			bb_get(wevidence,Wold),
			Wnew is Wold*W,
			bb_put(wevidence,Wnew),
			(Wnew>0 -> true; (!,fail))
			%bb_put(q,Rest)%,
			%writeln(lw(Wnew,QQ2,Rest))
		)
		;
			(
			sample(Distribution,Var),
			(
			(ground(Head2) )->
				(
					Head=Head2,Var=Var2,
					bb_put(q,Rest)%,
					%writeln(newq(QQ2,Rest))
				)
			;
				true
			)
			)
		)
	)
	;
		sample(Distribution,Var)
	),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_lw(Key,Temp,Head,_) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_lw2(Key,Temp,Head,_,_),	
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).

tabling_proof_query_backward_lw2(Key,Temp,Head,Distribution,_,Body) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward_lw(Key,Temp,Body,_).

	
tabling_proof_query_backward_lw2(Key,Temp,Head,_,Body) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_lw(Key,Temp,Body,_).


proof_query_backward_lw(Key,Temp,Head ~= Val,_) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
%	test_to_list(Q,Qlist),
 	proof_query_backward_lw(Key,Temp,Body,_),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	bb_get(q,QQ),
	(nth0(Pos,QQ,Head ~= _) ->
	(
		bb_get(q,QQ2),
		nth0(Pos,QQ2,Head2~=Var2,Rest),
		(
		(ground(Head2~=Var2))-> % Head and value ground in the original query
		(
		%	writeln(InQ= Head ~= Val),
			Var=Val,
			Var2=Val,
			bb_put(q,Rest),
			likelihood_weighting(Val,Distribution,W),
			bb_get(wevidence,Wold),
			Wnew is Wold*W,
			bb_put(wevidence,Wnew),
			(Wnew>0 -> true; (!,fail))
			
			%writeln(lw(Wnew,QQ2,Rest))
		)
		;
			(
			sample(Distribution,Var),
			(
			(ground(Head2) )->
				(
					Head=Head2,Var=Var2,
					bb_put(q,Rest)%,
					%writeln(newq(QQ2,Rest))
				)
			;
				true
			)
			)
		)
	)
	;
		sample(Distribution,Var)
	),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

% end proof_query_backward_lw temp
/*
% evidence needs to be asserted
check_evidence_backward(Key,PosEvidence,Wtot) :-
	bb_put(wevidence,1.0),
	forall(member(H,PosEvidence),
		(
%			trace,
			proof_query_backward_eval(Key,H,W),
%			writeln(proof_query_backward_eval(Key,H,W)),
			W>0,
			bb_get(wevidence,Wold),
%			writeln(bb_get(wevidence,Wold)),
			Wnew is Wold*W,
%			writeln(Wold*W),
			bb_put(wevidence,Wnew)
			
			
		)),
	bb_delete(wevidence,Wtot),!.
*/	
	
	
/*	
	(	% check positive evidence
		user:evidence(H,1),
		%member(H,PosEvidence),
		bb_get(wevidence,Wold),
		check_evidence_proof(Key,H,W),%proof_query_backward_eval(Key,Key,H,W),
		Wnew is Wold*W,
		bb_put(wevidence,Wnew),
		fail;
		true
	),
	bb_delete(wevidence,Wtot).
check_evidence_proof(Key,H,W) :-
	proof_query_backward_eval(Key,H,W),
	%writeln(proof_query_backward_eval(Key,Key,H,W)),
	!.
*/
log_likelihood_weighting(Val,D,LogW) :-
	likelihood_weighting(Val,D,W),
	LogW is log(W),!.

log_likelihood_weighting(Val,logfinite(L),W) :- % log weight
	member(W:Val,L),!.

log_likelihood_weighting(Val,logfinite(L),(-inf)) :- % log weight
	\+member(W:Val,L),!.

occurlist(E,[],0) :- !.
occurlist(E,[H|T],V) :- 
	(H==E ->
		Add=1
		;
		Add=0
	),
	occurlist(E,T,V2),
	V is V2+Add.

occurlistweight(E,[],0) :- !.
occurlistweight(E,[W:H|T],V) :- 
	(H==E ->
		Add=W
		;
		Add=0
	),
	occurlistweight(E,T,V2),
	V is V2+Add.

likelihood_weighting(Val,val(Val),1.0) :- !.
likelihood_weighting(Val,val(V),0.0) :- V\=Val,!.
	
likelihood_weighting(Val,uniform(L),W) :-
	uniformweight(Val,L,W),!.
	
/*	occurlist(Val,L,NVal), % memberchk(Val,L),
	length(L,N),
	W is NVal/N,!.
*/	
	
/*
likelihood_weighting(Val,uniform(L),0.0) :-
	\+memberchk(Val,L),
	!.
*/
likelihood_weighting(Val,beta(A,B),W) :-
	betaPdf(Val,A,B,W),!.
	
likelihood_weighting(Val,contUniform(A,B),W) :-
	Val>=A,
	Val=<B,
	W is 1/(B-A),!.

likelihood_weighting(Val,contUniform(A,B),0.0) :-
	(Val<A;Val>B),!.
	
likelihood_weighting(Val,finite(L),W) :-
	finiteweight(Val,L,W),!.
/*
likelihood_weighting(Val,finite(L),W) :-
	(
	member(W:Val,L) ->
		true
	;
		W=0
	),!.
	*/
likelihood_weighting(Val,propfinite(L),W) :-
	finiteweight(Val,L,W1),
	sum_prob(L,Sum),
	W is W1/Sum,!.	
	%occurlistweight(Val,L,W),!.%member(W:Val,L),!.
/*
likelihood_weighting(Val,finite(L),0.0) :-
	\+member(W:Val,L),!.
*/	
	
/*
likelihood_weighting(Val,logfinite(L),W) :- % log weight
	member(W:Val,L),!.

likelihood_weighting(Val,logfinite(L),(-inf)) :- % log weight
	\+member(W:Val,L),!.
*/
likelihood_weighting(Val,gaussian(M,Cov),W) :-
	test_to_list(Val,List),
	is_list(M),
	is_list(Cov),
	densityGaussian(M,Cov,List,W),!.

likelihood_weighting(Val,gaussian(M,Cov),W) :-
	test_to_list(Val,List),
	\+is_list(M),
	\+is_list(Cov),
	densityGaussian([M],[Cov],List,W),!.

likelihood_weighting(Val,student(Nu,Mean,Var),W) :-
	X is (Val-Mean)/sqrt(Var),
	studentPdf(Nu,X,W),!.

likelihood_weighting(Val,gamma(A,B),W) :-
	gammaPdf(A,B,Val,W),!.

likelihood_weighting(Val,invgamma(Alpha,Beta),W) :-
	B is 1/Beta,
	V is 1.0/Val,
	gammaPdf(Alpha,B,V,W),!.

likelihood_weighting(Val,dirichlet(Param),W) :-
	dirichletPdf(Param,Val,W),!.


listnelem([],0) :- !.
listnelem([A|T],L) :-
	L>0,
	L1 is L-1,
	listnelem(T,L1), !.

likelihood_weighting(Val,indepGaussians([(M,Cov)|T]),W) :-
	test_to_list(Val,List),
	is_list(M),
	is_list(Cov),
	length(M,Len),
	listnelem(NL,Len),
	append(NL,Suff,List),
	densityGaussian(M,Cov,NL,W1),
	length(Suff,Res),
	(
	Res>0 ->
		(
		test_to_list(Val2,Suff),
		likelihood_weighting(Val2,indepGaussians(T),W2),
		W is W1*W2
		)
	;
		W=W1
	),
	!.
	
likelihood_weighting(Val,poisson(Lambda),W) :-
	poissonPdf(Val,Lambda,W),!.

% complete likelihood_weighting

%%%



% findall for the forward chaining
%user:builtin(findall_forward(_,_,_)).
%findall_forward(X,Y,Z) :-
%	findall(X,query_proof(Y),Z).


% R is between the cumulative C
cumul([H|_],R,V,C) :- 
	H = C:V, 
	R=<C,
	!.
	
cumul([H|T],R,Val,C) :-
	H = P:_, 
	R2 is R-P, 
	cumul(T,R2,Val,C2),
	C is C2+P.

cumulpos([H|_],R,V,C,1) :- 
	H = C:V, 
	R=<C,
	!.
	
cumulpos([H|T],R,Val,C,Pos) :-
	H = P:_, 
	R2 is R-P, 
	cumulpos(T,R2,Val,C2,Pos1),
	C is C2+P,
	Pos is Pos1+1.
	
%TO TEST R is between the cumulative C
logcumul([H|_],R,V,C) :- 
	H = C:V, 
	R=<exp(C),
	!.
	
logcumul([H|T],R,Val,C) :-
	H = P:_, 
	R2 is R-exp(P), 
	logcumul(T,R2,Val,C2),
	C is C2+exp(P).

exactsampling(finite(Distribution),Val,P) :-
	!,
	member(P:Val,Distribution).

exactsampling(uniform(L),Val,W) :-
	!,
	member(Val,L),
	length(L,N),
	W is 1/N.

exactsampling(D,Val,0.1) :-
	between(1,10,I),
	sample(D,Val).

samplepos(propfinite(Distribution),Val,Pos) :-
	sum_prob(Distribution,Sum),
	X is random*Sum,
	Distribution\=[],
	cumulpos(Distribution,X,Val,_,Pos), !.	
% sample a value from a given distribution
sample(propfinite(Distribution),Val) :-
	sum_prob(Distribution,Sum),
	X is random*Sum,
	Distribution\=[],
	cumul(Distribution,X,Val,_), !.
	
sample(finite(Distribution),Val) :-
%	samplefinite(Distribution,Val),!.
	X is random,
	Distribution\=[],
	cumul(Distribution,X,Val,_), !.

sample(logfinite(Distribution),Val) :-
	X is random,
	Distribution\=[],
	logcumul(Distribution,X,Val,_),!.

sample(val(Val),Val) :-
	!.
	
sample(uniform([Val]),Val) :-
	!.

sample(uniform(Distribution),Val) :-
	Distribution\=[],
	draw_uniform(Distribution,Val), !.

sample(beta(Alpha,Beta),Val) :-
	dirichlet([Alpha,Beta],[Val,Val2]), !.

sample(student(Nu),Val) :-
	student(Nu,Val), !.

sample(student(Nu,Mean,Var),Val) :-
	student(Nu,StVal),
	Val is StVal*sqrt(Var)+Mean, !.

sample(gamma(Alpha,Beta),Val) :-
	gamma(Alpha,Beta,Val), !.

sample(invgamma(Alpha,Beta),Val) :-
	B is 1/Beta,
	gamma(Alpha,B,Precision),
	Val is 1.0/Precision,!.
			
% sample continuous and uniform distribution
sample(contUniform(A,B),Val) :-
	sample_uniform(A,B,Val),
	 !.

sample(contUniform([(A,B)]),Val) :-
	sample_uniform(A,B,Val),
	 !.
	  
sample(contUniform([(A,B)|T]),(Val,Val2)) :-
	sample(contUniform(T),Val2),
	sample_uniform(A,B,Val),
	 !.

sample(contUniform(A,B,C,D),(Val,Val2)) :-
	sample_uniform(A,B,Val),
	sample_uniform(C,D,Val2),
	 !.

sample(contUniform(A,B,C,D,E,F),(Val,Val2,Val3)) :-
	sample_uniform(A,B,Val),
	sample_uniform(C,D,Val2),
	sample_uniform(E,F,Val3),
	 !.
	  
sample(contUniform(A,B,C,D,E,F,G,H),(Val,Val2,Val3,Val4)) :-
	sample_uniform(A,B,Val),
	sample_uniform(C,D,Val2),
	sample_uniform(E,F,Val3),
	sample_uniform(G,H,Val4),
	 !.

sample(optimalProposal(FX,SigmaV,C,SigmaW,Y),Tuple) :-
	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,W), % args: 1: f(x_{t-1}), 2: sigma_v, 3: C, 4: sigma_w, 5: y, 6: sampled state, 7: weight
	test_to_list(Tuple,Val),
	!.

sample(indepOptimalProposals(List),Tuple) :-
	indepOptimalProposals(List,Val,W),
	test_to_list(Tuple,Val),
	!.

indepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)],Val,W) :-
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,W),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
%	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,W),
	!.
	
indepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)|T],Ris,W) :-
	indepOptimalProposals(T,SubList,WT),
%	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,WH),
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,WH),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
	W is WT*WH,
	list_concat([Val,SubList],Ris),
	!.

sample(logIndepOptimalProposals(List),[Tuple,W]) :-
	logIndepOptimalProposals(List,Val,W),
	test_to_list(Tuple,Val),
	!.

logIndepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)],Val,LogW) :-
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,W),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
	LogW is log(W),!.
	
logIndepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)|T],Ris,W) :-
	logIndepOptimalProposals(T,SubList,WT),
%	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,WH),
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,WH),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
	W is WT+log(WH),
%	writeln(W is WT+log(WH)),
	list_concat([Val,SubList],Ris),!.

sample(gaussian([Hm|Mean],[Hc|Cov]),Tuple) :-
	!,
	gaussian([Hm|Mean],[Hc|Cov],Val),
	test_to_list(Tuple,Val),
	!.
	
sample(gaussian(Mean,Var),Val) :-
	normalgsl(Mean,Var,Val), %gaussian([Mean],[Var],[Val]), % test gaussian findall(Val,(between(1,10000,_),sample(gaussian(3,1.5),Val)),L),sum_list(L,S),M is S/10000,findall(Variance,(member(E,L),Variance is (E-M)^2),LV),sum_list(LV,VarSum),Var2 is VarSum/9999.
	!.

checkline([C],[],0,C) :-
	!.
	
checkline([C1|H],[A|B],X,C) :-
	checkline(H,B,X2,C),
	X is X2+C1*A.

% Test open('data.txt','write',S),findall(A,(between(1,1000,I),distributionalclause:sample(gaussian_cutmax([0],[0.1],(1,0.1)),A),write(S,A),write(S,' '),write(S,1),nl(S)),_),close(S).
% open('data.txt','write',S),findall(A,(between(1,1000,I),distributionalclause:sample(gaussian_cutmax([0,0],[0.1,0,0,0.1],(1,1,0.0)),(A,B)),write(S,A),write(S,' '),write(S,B),nl(S)),_),close(S).
sample(gaussian_cutmax([Hm|Mean],[Hc|Cov],Limit1),Tuple) :-
	!,
	gaussian([Hm|Mean],[Hc|Cov],Val),
	test_to_list(Limit1,Limit),
	checkline(Limit,Val,X,C),
	(
		X>C ->
			sample(gaussian_cutmax([Hm|Mean],[Hc|Cov],Limit1),Tuple)
		;
			test_to_list(Tuple,Val)
	),!.

sample(gaussian_cutmin([Hm|Mean],[Hc|Cov],Limit1),Tuple) :-
	!,
	gaussian([Hm|Mean],[Hc|Cov],Val),
	test_to_list(Limit1,Limit),
	checkline(Limit,Val,X,C),
	(
		X<C ->
			sample(gaussian_cutmin([Hm|Mean],[Hc|Cov],Limit1),Tuple)
		;
			test_to_list(Tuple,Val)
	),!.
	
sample(gaussian_cutmax(Mean,Var,Limit),Val) :-
	gaussian([Mean],[Var],[Val1]),
	Val is min(Val1,Limit),
	!.

sample(gaussian_cutmin(Mean,Var,Limit),Val) :-
	gaussian([Mean],[Var],[Val1]),
	Val is max(Val1,Limit),
	!.
	 
sample(dirichlet(A),List) :-
	dirichlet(A,List),
	 !.

% sample couples (X,Y) from 2 independent gaussians
sample(indepGaussians(List),Tuple) :-
	indepGaussians(List,Val),
	test_to_list(Tuple,Val),
	!.

indepGaussians([(M,C)],Val) :-
	gaussian(M,C,Val),
	!.
	
indepGaussians([(M,C)|T],Ris) :-
	indepGaussians(T,SubList),
	gaussian(M,C,Val),
	list_concat([Val,SubList],Ris),
	!.
	
sample(indepGaussians_cutmin([(M,C)],[Limit]),[Val]) :-
	sample(gaussian_cutmin(M,C,Limit),Val),!.


sample(indepGaussians_cutmin([(M,C)|T],[Limit|H]),Ris) :-
	sample(indepGaussians_cutmin(T,H),SubList),
	sample(gaussian_cutmin(M,C,Limit),Val),
	list_concat([[Val],SubList],Ris),!.
	

sample(indepGaussians_cutmax([(M,C)],[Limit]),[Val]) :-
	sample(gaussian_cutmax(M,C,Limit),Val),!.


sample(indepGaussians_cutmax([(M,C)|T],[Limit|H]),Ris) :-
	sample(indepGaussians_cutmax(T,H),SubList),
	sample(gaussian_cutmax(M,C,Limit),Val),
	list_concat([[Val],SubList],Ris),!.

/*	
sample(gaussian(MeanX,VarX,MeanY,VarY),(ValX,ValY)) :-
	normal2(MeanX,VarX,MeanY,VarY,ValX,ValY),
	 !.

% sample 4 values from 4 independent gaussians	
sample(gaussian(Mean1,Var1,Mean2,Var2,Mean3,Var3,Mean4,Var4),(Val1,Val2,Val3,Val4)) :-
	normal(Mean1,Var1,Val1),normal(Mean2,Var2,Val2),
	normal(Mean3,Var3,Val3),normal(Mean4,Var4,Val4),
	 !.
*/
sample(poisson(L),Val) :-
	poisson(L,Val), !.


%%%%%% magic set %%%%%%%%%%

% Magic transformation
magic :-
	retractall(user:hardclause(_,_,_)),
	retractall(user:distributionalclause(_,_,_,_)),
	(
		inference(true)
		->
			(
			%retractall(user:hardclause(_,_,_)),
			%retractall(user:distributionalclause(_,_,_,_)),
			magic_hardclause,
			magic_distributionalclause
			)
		;
		(
			inference(particle)
			->
			(
				expansion_magic_off
				%magic_particlefilter
			)
			;
				expansion_magic_off
		)
	).

timesyntax(H:t,current(H)) :-
	!.

timesyntax(findall_forward(A,B,C),findall_forward(A,BB,C)) :-
	timesyntax(B,BB),
	!.


timesyntax(H:t+1,next(H)) :-
	!.

timesyntax(H:0,prior(H)) :-
	!.

timesyntax(H:t ~= V,current(H) ~= V) :-
	!.
	
timesyntax(H:0 ~= V,prior(H) ~= V) :-
	!.
	
timesyntax(H:t+1 ~= V,next(H) ~= V) :-
	!.

timesyntax((H,H2),(HH,HH2)) :-
	timesyntax(H,HH),
	timesyntax(H2,HH2),
	!.
timesyntax(\+H,\+HH) :-
	timesyntax(H,HH),
	!.

timesyntax(H,H) :-
	H\= (_,_),
	H\= _:0,
	H\= _:t,
	H\= _:t+1,
	H\= _:t ~= _,
	H\= _:t+1 ~= _,
	!.

% not complete
maxrank(A,0) :-
	user:builtin(A),!.

maxrank((A,B),Rank) :-
	maxrank(A,R),
	maxrank(B,R2),
	Rank is max(R,R2),!.

compute_rank(Head,Body,Rank) :-
	maxrank(Body,R1),
	Rank is R1+1,!.
%

containscurrent(current(_)) :-
	!.

containscurrent(current(_) ~= _) :-
	!.
	
containscurrent((A,B)) :-
	containscurrent(A),!.

containscurrent((A,B)) :-
	containscurrent(B),!.


containscurrent(findall_forward(A,B,C)) :-
	!,containscurrent(A),!.

containscurrent(findall_forward(A,B,C)) :-
	!,containscurrent(B),!.


containsnext(next(_)) :-
	!.

containsnext(next(_) ~= _) :-
	!.
	
containsnext((A,B)) :-
	containsnext(A),!.

containsnext((A,B)) :-
	containsnext(B),!.

containsnext(findall_forward(A,B,C)) :-
	!,containsnext(A),!.

containsnext(findall_forward(A,B,C)) :-
	!,containsnext(B),!.


bodycurrent2next(current(A),next(A)) :- !.
bodycurrent2next(current(A) ~= V,next(A) ~= V) :- !.

bodycurrent2next((A,B),(NA,NB)) :-
	!,
	bodycurrent2next(A,NA),
	bodycurrent2next(B,NB).

bodycurrent2next(findall_forward(A,B,C),findall_forward(AA,BB,CC)) :-
	!,
	bodycurrent2next(A,AA),
	bodycurrent2next(B,BB),!.

bodycurrent2next(A,A) :- !.
	
expansion_magic_off :-
	(
		user:(H ~ D),
		H\= _:_,
		assert(user:distributionalclause(H,D,true,0)),
		fail;
		true
	),
	(
		user:(H:0 ~ D),
		assert(user:distributionalclause(prior(H),D,true,0)),
		fail;
		true
	),
	(
		user:(H:t ~ D),
		assert(user:distributionalclause(current(H),D,true,0)),
		fail;
		true
	),
	(
	current2nextcopy(true)->
	( % copying H:t ~ D to H:t+1 ~ D
		user:(H:t ~ D),
		assert(user:distributionalclause(next(H),D,true,0)),
		assert(user:distributionalclausecopied(next(H),D,true,0)),
		fail;
		true
	);true
	),
	(
		user:(H:t+1 ~ D),
		assert(user:distributionalclause(next(H),D,true,1)),
		fail;
		true
	),
	
	(
		user:(H ~ D:=B),
		H\= _:_,
		assert(user:distributionalclause(H,D,B,0)),
		fail;
		true
	),
	(
		user:(H:0 ~ D:=B),
		timesyntax(B,BB),
		assert(user:distributionalclause(prior(H),D,BB,0)),
		fail;
		true
	),
	(
		user:(H:t ~ D:=B),
		timesyntax(B,BB),
		assert(user:distributionalclause(current(H),D,BB,0)),
		fail;
		true
	),
	(
	current2nextcopy(true)->
	( % copying H:t ~ D:=B_t to H:t+1 ~ D:=B_t+1
		user:(H:t ~ D:=B),
		B\=belief(_),
		B\=(belief(_),_),
		timesyntax(B,BB),
		bodycurrent2next(BB,BBB),
		assert(user:distributionalclause(next(H),D,BBB,0)),
		assert(user:distributionalclausecopied(next(H),D,BBB,0)),
		fail;
		true
	);true
	),
	(
		user:(H:t+1 ~ D:=B),
		timesyntax(B,BB),
		assert(user:distributionalclause(next(H),D,BB,1)),
		fail;
		true
	),
	(
		user:(H := B),
		H\=_~_,
		H\=_:_,
		assert(user:hardclause(H,B,0)),
		fail;
		true
	),
	(
		user:(H:0 :=B),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(prior(H),BB,0)),
		fail;
		true
	),
	(
		user:(H:t :=B),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(current(H),BB,0)),
		fail;
		true
	),
	(
	current2nextcopy(true)->
	( % copying H:t :=B_t to H:t+1 :=B_t+1
		user:(H:t :=B),
		B\=belief(_),
		B\=(belief(_),_),
		H\=_~_,
		timesyntax(B,BB),
		bodycurrent2next(BB,BBB),
		assert(user:hardclause(next(H),BBB,0)),
		assert(user:hardclausecopied(next(H),BBB,0)),
		fail;
		true
	);true
	),
	(
		user:(H:t+1 :=B),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(next(H),BB,1)),
		fail;
		true
	),
	
	(
	
		user:(H ~ D pr X),
		H\=_:_,
		assert(user:distributionalclause(H,D,true,X)),
		fail;
		true
	),
	(
	
		user:(H:0 ~ D pr X),
		assert(user:distributionalclause(prior(H),D,true,X)),
		fail;
		true
	),
	(
	
		user:(H:t ~ D pr X),
		assert(user:distributionalclause(current(H),D,true,X)),
		fail;
		true
	),
	(
	current2nextcopy(true)->
	(% copying H:t ~ D pr X to H:t+1 ~ D pr X
	
		user:(H:t ~ D pr X),
		assert(user:distributionalclause(next(H),D,true,X)),
		assert(user:distributionalclausecopied(next(H),D,true,X)),
		write('warning, check priority of clauses current to next: '),writeln((H:t ~ D pr X)),
		fail;
		true
	);true
	),
	(
	
		user:(H:t+1 ~ D pr X),
		assert(user:distributionalclause(next(H),D,true,X)),
		fail;
		true
	),
	
	(
		user:(H ~ D := B pr X),
		H\=_:_,
		assert(user:distributionalclause(H,D,B,X)),
		fail;
		true
	),
	(
		user:(H:0 ~D:=B pr X),
		timesyntax(B,BB),
		assert(user:distributionalclause(prior(H),D,BB,X)),
		fail;
		true
	),
	(
		user:(H:t ~D:=B pr X),
		timesyntax(B,BB),
		assert(user:distributionalclause(current(H),D,BB,X)),
		fail;
		true
	),
	(
	current2nextcopy(true)->
	( % copying H:t ~D:=B_t pr X to H:t+1 ~D:=B_t+1 pr X
		user:(H:t ~D:=B pr X),
		B\=belief(_),
		B\=(belief(_),_),
		timesyntax(B,BB),
		bodycurrent2next(BB,BBB),
		assert(user:distributionalclause(next(H),D,BBB,X)),
		assert(user:distributionalclausecopied(next(H),D,BBB,X)),
		write('warning, check priority of clauses current to next: '),writeln((H:t ~D:=B pr X)),
		fail;
		true
	);true
	),
	(
		user:(H:t+1 ~D:=B pr X),
		timesyntax(B,BB),
		assert(user:distributionalclause(next(H),D,BB,X)),
		fail;
		true
	),
	
	(
		user:(H := B pr X),
		H\=_~_,
		H\=_:_,
		assert(user:hardclause(H,B,X)),
		fail;
		true
	),
	(
		user:(H:0 :=B pr X),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(prior(H),BB,X)),
		fail;
		true
	),
	(
		user:(H:t :=B pr X),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(current(H),BB,X)),
		fail;
		true
	),
	(
	current2nextcopy(true)->
	( % copying H:t :=B pr X to H:t+1 :=B_t+1 pr X
		user:(H:t :=B pr X),
		B\=belief(_),
		B\=(belief(_),_),
		H\=_~_,
		timesyntax(B,BB),
		bodycurrent2next(BB,BBB),
		assert(user:hardclause(next(H),BBB,X)),
		assert(user:hardclausecopied(next(H),BBB,X)),
		write('warning, check priority of clauses current to next: '),writeln((H:t :=B pr X)),
		fail;
		true
	);true
	),
	(
		user:(H:t+1 :=B pr X),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(next(H),BB,X)),
		fail;
		true
	).


add_call(A ~= _,callmagic(dist_eq(A))) :-
	!.
add_call(\+(A ~= _),callmagic(dist_eq(A))) :-
	!.

add_call(A,callmagic(A)) :-
	 A \= (\+Something),
	 !.

add_call(A,callmagic(Something)) :-
	 A = (\+Something),
	 !.

/*
add_call(A,Call_A) :-
	A=..[F|Arg],
	atomic_concat(['call_',F],F2),
	Call_A=..[F2|Arg].
*/
split_body(Body,T,Prefix) :-
	test_to_list(Body,L),
	append(Part1,[T|_],L),
	test_to_list(Prefix,Part1).


% convert from (A,B,C...) to [A,B,C,...]
test_to_list(true,[]) :-
	!.
test_to_list(A,[A]) :-
	\+ A=(_,_),
	!.
test_to_list((A,T1),[A|T2]) :-
	test_to_list(T1,T2).

magic_hardclause(Head,Body,Pr) :-
	test_to_list(Body,B),
	findManage(B,NewB),
	magic_set_hard(Head,NewB,0),
	(

		add_call(Head,Head_Call),
		(
			Body == true
			->
				NewBody=Head_Call
				;
				NewBody=(Head_Call,Body)
		)
			
	),
	%write((Head := NewBody pr Pr)),nl,
	assert(user:hardclause(Head,NewBody,Pr)).

magic_distributionalclause(Head,D,Body,Pr) :-
	(
		add_call(Head ~= X,Head_Call),
		(
			Body == true
			->
				NewBody=Head_Call;
				NewBody=(Head_Call,Body)
		)
		
	),
	%write((Head ~ D := NewBody pr Pr)),nl,
	assert(user:distributionalclause(Head,D,NewBody,Pr)),
	test_to_list(Body,B),
	findManage(B,NewB),
	magic_set_hard(Head ~= X,NewB,0).

% TO ASK! NOT FINISHED findall forward with magic (Temporal solution)
findManage(Body,NewBody) :-
	%test_to_list(B,Body),
	(
		append(P,[findall_forward(A,T,C)|Tail],Body)
		->
		(
			copy_term(T,T2),
			test_to_list(T2,ListT),
			append(P,ListT,P2),
			append(P2,Tail,NewBody1),% remove findall_forward append(P2,[findall_forward(A,T,C)|Tail],NewBody)
			remove_builtin(NewBody1,NewBody) % remove builtin TO CHECK!
		);
		NewBody=Body
	).
	%test_to_list(NewBody,NewBody1).

magic_hardclause :-
	(
		user:(Head := Body),
		Head\=V~D,
		%write((Head := Body)),nl,
		
		magic_hardclause(Head,Body,0),
		%nl,
		fail;
		true
	),
	(
		user:(Head := Body pr X),
		Head\=V~D,
		%write((Head := Body pr X)),nl,
		
		magic_hardclause(Head,Body,X),
		%nl,
		fail;
		true
	).
	
magic_distributionalclause :-
	(
		user:(Head ~ D := Body),
		%write((Head ~ D := Body)),nl,
		
		magic_distributionalclause(Head,D,Body,0),
		%nl,
		fail;
		true
	),
	(
		user:(Head ~ D),
		%write((Head ~ D)),nl,
		magic_distributionalclause(Head,D,true,0),
		%nl,
		fail;
		true
	),
	(
		user:(Head ~ D := Body pr X),
		%write((Head ~ D := Body pr X)),nl,
		
		magic_distributionalclause(Head,D,Body,X),
		%nl,
		fail;
		true
	),
	(
		user:(Head ~ D pr X),
		%write((Head ~ D pr X)),nl,
		magic_distributionalclause(Head,D,true,X),
		%nl,
		fail;
		true
	).




remove_builtin([true],[true]) :-
	!.

remove_builtin([H],[]) :-
	user:builtin(H),
	!.

remove_builtin([H],[H]) :-
	\+user:builtin(H),
	!.

remove_builtin([H|T],NewBody) :-
	(
		user:builtin(H)
		->
		(
			remove_builtin(T,NewBody)

		)
		;
		(
			remove_builtin(T,NewT),
			NewBody=[H|NewT]
		)
	).

magic_set_hard(Head,[],Pr) :-
	!.

magic_set_hard(Head,Body,Pr) :-
	(
		append(Prefix,[Last|[]],Body),
		(
			(user:builtin(Last))%;remove_magic(Last))
			->
			true
			;
			(
				add_call(Last,Call_Last),
				(
					%remove_magic(Head) ->
					%	test_to_list(NewBody,Prefix)
					%;
						(
							add_call(Head,Call_Head),
							test_to_list(NewBody,[Call_Head|Prefix])
						)
				),
				%write((Call_Last := NewBody)),nl,
				assert(user:hardclause(Call_Last,NewBody,Pr))
			)
		)
	),
	magic_set_hard(Head,Prefix,Pr),
	!.


init_query(Key,_) :-
	inference(false),
	!.
init_query(Key,(H;T)) :-
	!,
	init_query(Key,H),
	init_query(Key,T).
init_query(Key,(H,T)) :-
	!,
	init_query(Key,H),
	init_query(Key,T).
init_query(Key,\+ Atom) :-
	!,
	add_call(Atom,CallAtom),
	recorda(Key,CallAtom,_).
	
init_query(Key,Atom) :-
	add_call(Atom,CallAtom),
	recorda(Key,CallAtom,_).


% add callmagic for the evidence
init_query_list(Key,[]).

init_query_list(Key,_) :-
	inference(false),
	!.

init_query_list(Key,[H|T]) :-
	init_query(Key,H),
	init_query_list(Key,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_evidence([],V) :- !.

	
assert_evidence((H,T),V) :-
	assert(user:evidence(H,V)),
	assert_evidence(T,V),!.

assert_evidence([H|T],V) :-
	assert(user:evidence(H,V)),
	assert_evidence(T,V),!.
	
assert_evidence(A,V) :-
	A\=[],
	A\=[H|T],
	assert(user:evidence(A,V)),
	assert_evidence(T,V),!.

assert_impossible_evidence(Key,[]).

assert_impossible_evidence(Key,[H|T]) :-
	recorda(Key,H,_),
	assert_impossible_evidence(Key,T).	
	
/*
assert_evidence(true) :-
	!.
assert_evidence((A,B)) :-
	!,
	assert_evidence(A),
	assert_evidence(B).

assert_evidence(\+H) :-
	assert(user:evidence(H,0)).

assert_evidence(H) :-
	assert(user:evidence(H,1)).
*/

% Sum = Distr1 + Distr2 * Weight
sum_distrib(finite(Distr1),finite(Distr2),Weight,finite(Sum)) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),W is W1+W2*Weight),NewDist),
	%write(NewDist),nl,
	findall(W:Val,(member(W:Val,Distr1),\+member(_:Val,Distr2)),NewDist2),
	%write(NewDist2),nl,
	findall(W:Val,(member(W2:Val,Distr2), W is W2*Weight,\+member(_:Val,Distr1)),NewDist3),
	%write(NewDist3),nl,
	append(NewDist,NewDist2,TOT),
	append(NewDist3,TOT,Sum).

% Prod = Distr1 * Distr2 * Normalicost
prod_distrib(finite(Distr1),finite(Distr2),finite(ProdNorm)) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),W is W1*W2),NewDist),
	%write(NewDist),nl,
	findall(W:Val,(member(W:Val,Distr1),\+member(_:Val,Distr2)),NewDist2),
	%write(NewDist2),nl,
	append(NewDist,NewDist2,Prod),
	normalize(Prod,ProdNorm).


sum_list_multidim(Distr1,Distr2,Sum) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),sum_multidim(W1,W2,W)),NewDist),
	%write(NewDist),nl,
	findall(W:Val,(member(W:Val,Distr1),\+member(_:Val,Distr2)),NewDist2),
	%write(NewDist2),nl,
	findall(W:Val,(member(W:Val,Distr2),\+member(_:Val,Distr1)),NewDist3),
	%write(NewDist3),nl,
	append(NewDist,NewDist2,TOT),
	append(NewDist3,TOT,Sum).
			
% sum of probabilities in a finite distribution
sum_prob([],0.0) :-
	!.

sum_prob([P:V|T],Sum) :-
	sum_prob(T,Sum2),
	Sum is Sum2+P.

sum_multidim(A,B,Sum) :-
	A\=(_,_),
	B\=(_,_),
	Sum is A+B,
	!.

sum_multidim((A,B),(C,D),(Sum,Sum2)) :-
	sum_multidim(B,D,Sum2),
	Sum is A+C.

diffsquared_multidim(W,A,B,Diff) :-
	A\=(_,_),
	B\=(_,_),
	Diff is W*(A-B)^2,
	!.

diffsquared_multidim(W,(A,B),(C,D),(Diff,Diff2)) :-
	diffsquared_multidim(W,B,D,Diff2),
	Diff is W*(A-C)^2.

div_scalar_multidim(P,Tot,P2) :-
	P\=(_,_),
	P2 is P/Tot,
	!.
	
div_scalar_multidim((P,Tail),Tot,(P2,NewDistrTail)) :-
	P2 is P/Tot,
	div_scalar_multidim(Tail,Tot,NewDistrTail),
	!.

prod_scalar_multidim(P,Tot,P2) :-
	P\=(_,_),
	P2 is P*Tot,
	!.
	
prod_scalar_multidim((P,Tail),Tot,(P2,NewDistrTail)) :-
	P2 is P*Tot,
	prod_scalar_multidim(Tail,Tot,NewDistrTail),
	!.
		
divide_multidim(Distr1,Distr2,NewDist) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),div_scalar_multidim(W1,W2,W)),NewDist).

% products element of a list
product_list([],1) :-
	!.
	
product_list([H|T],P) :-
	product_list(T,SubP),
	P is SubP*H.

product_wlist([],[],1) :-
	!.
	
product_wlist([W:H|T],[H|TT],P) :-
	product_wlist(T,TT,SubP),
	P is SubP*W.	

% divide probabilities by Tot in a finite distribution
divideby([P:V],Tot,NewDistr) :-
	P2 is P/Tot,
	NewDistr=[P2:V],
	!.
	
divideby([P:V|Tail],Tot,NewDistr) :-
	P2 is P/Tot,
	divideby(Tail,Tot,NewDistrTail),
	NewDistr=[P2:V|NewDistrTail],
	!.

%(clean distribution removing values with prob. 0)

cleanDistribution([],[],_) :- !.
	
cleanDistribution([P:V|Tail],NewDistr,Limit) :-
	cleanDistribution(Tail,NewDistrTail,Limit),
	(
		P>Limit ->
		NewDistr=[P:V|NewDistrTail]
		;
		NewDistr=NewDistrTail
	).

findmax(P:V,[P:V]) :- !.
findmax(PM:M,[PH:H|T]) :-
	findmax(PMT:MT,T),
	(
		PMT>PH ->
			(
			PM=PMT,
			M=MT
			)
		;
		(
			PM=PH,
			M=H
		)
	).

findmin(P:V,[P:V]) :- !.
findmin(PM:M,[PH:H|T]) :-
	findmin(PMT:MT,T),
	(
		PMT<PH ->
			(
			PM=PMT,
			M=MT
			)
		;
		(
			PM=PH,
			M=H
		)
	).
	
% multiply probabilities by W in a finite distribution
%(clean distribution removing values with prob. 0)
multiplyby([P:V],W,NewDistr) :-
	P2 is P*W,
	(
		P2>0 ->
		NewDistr=[P2:V]
		;
		NewDistr=[]
	),
	!.
	
multiplyby([P:V|Tail],W,NewDistr) :-
	P2 is P*W,
	multiplyby(Tail,W,NewDistrTail),
	(
		P2>0 ->
		NewDistr=[P2:V|NewDistrTail]
		;
		NewDistr=NewDistrTail
	),	
	!.

% normalize a finite distribution
normalize([],[]) :- !.

normalize(Distr,DistrNorm) :-
	%write(Distr),nl,
	sum_prob(Distr,Sum),
	divideby(Distr,Sum,DistrNorm).

sample_lookahead(Key,Head,poisson(A),poisson(A),1.0) :- 
	writeln('warning! not implemented'),!.
% TO CHECK! NOT VERIFIED
sample_lookahead(Key,Head,gaussian(A,B),Distr,W) :- 
	\+is_list(A),
	\+is_list(B),
	!,
	(
		(user:evidence(Head ~= V,1), ground(V))
		->
		(
			Distr=uniform([V]),
			%W is 1/(sqrt(2*pi*B))*exp(-(X-A)*(X-A)/(2*B)) % TO CHECK! NOT VERIFIED
			densityGaussian([A],[B],[V],W),
			writeln('warning! not tested')
		)
		;
		(
			Distr=gaussian(A,B),
			W is 1.0
		)
	),
	!.

sample_lookahead(Key,Head,contUniform(A,B),Distr,1.0) :- 
	!,
	(
		(user:evidence(Head ~= V,1), ground(V))
		->
		(
			Distr=uniform([V]),
			writeln('warning! not tested')
		)
		;
		Distr=contUniform(A,B)
	),
	!.	
	
% TO CHECK! NOT VERIFIED
/*
sample_lookahead(Key,Head,uniform(A,B,C,D),Distr,1.0) :- 
	!,
	(
		(user:evidence(Head ~= V,1), ground(V))
		->
		(
			Distr=uniform([V])
		)
		;
		Distr=uniform(A,B,C,D)
	),
	!.	
*/

sample_lookahead(Key,Head,bigaussian((A,B),(C,D,E,F)),bigaussian((A,B),(C,D,E,F)),1.0) :-
	writeln('warning! not tested'),!.
sample_lookahead(Key,Head,dirichlet(A),dirichlet(A),1.0) :-
	writeln('warning! not tested'),!.
sample_lookahead(Key,Head,gaussian(A,B,C,D),gaussian(A,B,C,D),1.0) :-
	writeln('warning! not tested'),!.
	
sample_lookahead(Key,Head,gaussian(A,B),gaussian(A,B),1.0) :-
	writeln('warning! not tested'),
	is_list(A),
	is_list(B),!.
	
sample_lookahead(Key,Head,uniform(A,B,C,D),uniform(A,B,C,D),1.0) :-
	!.

sample_lookahead(Key,Head,gaussian(A,B,C,D,E,F,G,H),gaussian(A,B,C,D,E,F,G,H),1.0) :-
	writeln('warning! not tested'),!.
sample_lookahead(Key,Head,uniform(A,B,C,D,E,F,G,H),uniform(A,B,C,D,E,F,G,H),1.0) :-
	!.
	
sample_lookahead(Key,Head,uniform(Distribution),uniform(NewDistr),Weight) :-
	!,
	(
		user:evidence(Head ~= V,1)
		->
		(
			ground(V)
			->
			(
				NewDistr=[V],
				length(Distribution,S),
				Weight is 1/S
			);
			(
				Weight = 1.0,
				NewDistr = Distribution
			)
		)
		;
		(
			bb_put(distr,Distribution),
			(
				user:evidence(Head ~= V,0),
				bb_get(distr,D),
				delete(D,V,D2), % remove false evidence from distribution
				bb_put(distr,D2),
				fail;
				true
			),
			bb_delete(distr,Dist),
			remove_inconsistent_value(Key,Head,uniform(Dist),NewDistr,SumRemoved,3),
			
			length(NewDistr,Snew),
			length(Distribution,Sold),
			Weight is Snew/Sold
		)
	),
	!.
	
sample_lookahead(Key,Head,finite(Distribution),finite(NewDistr),Weight) :-
	!,
	(
		user:evidence(Head ~= V,1)
		->
		(
			ground(V)
			->
			(
				NewDistr=[1:V],
				member(Weight:V,Distribution)
				% Weight is P
			);
			(
				Weight = 1.0,
				NewDistr = Distribution
			)
		)
		;
		(
			bb_put(distr,Distribution),
			bb_put(w,1.0),
			(	% check false evidence
				user:evidence(Head ~= V,0),
				bb_get(distr,D),
				member(Prob:V,D),
				delete(D,Prob:V,D2), % remove false evidence from distribution
				bb_put(distr,D2),
				bb_get(w,W1),
				W2 is W1-Prob, % update weight
				bb_put(w,W2),
				fail;
				true
			),
			bb_delete(distr,D3),
			remove_inconsistent_value(Key,Head,finite(D3),NewDistrNotNorm,SumP,3),
			normalize(NewDistrNotNorm,NewDistr),
			bb_delete(w,W4),
			Weight is W4-SumP
		)
	),
	!.



%%% evidence proof
evidence_proof_exists_maybe(Key,Depth) :-
	\+ (user:evidence(Atom,1), \+ proof_exists_maybe(Key,Atom,Depth)).

proof_exists_maybe(Key,true,_Depth) :-
	!.
	
proof_exists_maybe(Key,(A,B),Depth) :-
	!,
	proof_exists_maybe(Key,A,Depth),
	proof_exists_maybe(Key,B,Depth).

/* TO CHECK
proof_exists_maybe(A is integer(B),_Depth) :-
	!,
	integer(A),
	B=A,
	!.
*/
proof_exists_maybe(Key,between(A,B,C),_Depth) :-
	\+ground(B),
	A=<C,
	!.

proof_exists_maybe(Key,A is B,_Depth) :-
	\+ground(B),
	!.

proof_exists_maybe(Key,callmagic(_),_Depth) :-
	!.

% TO CHECK
proof_exists_maybe(Key,findall_forward(X,Y,Z),Depth) :-
	findall(X,proof_exists_maybe(Key,Y,0),Z),
	!.

% TO CHECK
proof_exists_maybe(Key,min_list(A,B),_Depth) :-
	\+ground(A),
	!.
proof_exists_maybe(Key,max_list(A,B),_Depth) :-
	\+ground(A),
	!.
	
proof_exists_maybe(Key,A,_Depth) :-
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_exists_maybe(Key,\+A,_Depth) :-
	\+recorded(Key,A,_).
	
proof_exists_maybe(Key,A,_Depth) :-
	recorded(Key,A,_).

proof_exists_maybe(Key,Head ~= Var,Depth) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	Distribution=finite(L)
		->
		 (	% TO FIX! there might be problems with non ground Head/Distribution
			  \+recorded(Key,Head ~= Var1,_),
			  %copy_term(Var,Var2), % to avoid unification
			  member(_:Var,L)%memberchk(_:Var2,L) % member(_:Var,L)
		 );
		 (
			 Distribution=uniform(L)
			 ->
			 (
				\+recorded(Key,Head ~= Var1,_),
				%copy_term(Var,Var2), % to avoid unification
				member(Var,L)%memberchk(Var2,L) % member(Var,L)
			 );
			 true
		 )
	),
	Depth2 is Depth-1,
	(
		Depth2<1
		->
	 	true;
		(
			%write(Body),nl,
			proof_exists_maybe(Key,Body,Depth2)	
		)
	).
	
proof_exists_maybe(Key,A,Depth) :-
	user:hardclause(A,Body,_),
	\+recorded(Key,A,_),
	Depth2 is Depth-1,
	(
		Depth2<1
		->
		true;
		(
			%write(Body),nl,
			proof_exists_maybe(Key,Body,Depth2)	
		)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

remove_inconsistent_value(Key,Head,finite([]),[],0.0,Depth).
remove_inconsistent_value(Key,Head,uniform([]),[],0,Depth).

remove_inconsistent_value(Key,Head,finite([P:V|H]),NewDistr,SumPremoved,Depth) :-
	(
		check_value(Key,Head ~= V,Depth)
		->
		(
			remove_inconsistent_value(Key,Head,finite(H),NewDistrH,SumPremoved1,Depth),
			NewDistr=[P:V|NewDistrH],
			SumPremoved is SumPremoved1
		);
		(
			remove_inconsistent_value(Key,Head,finite(H),NewDistrH,SumPremoved1,Depth),
			NewDistr=NewDistrH,
			SumPremoved is SumPremoved1+P
		)
	).

remove_inconsistent_value(Key,Head,uniform([V|H]),NewDistr,Nremoved,Depth) :-
	(
		check_value(Key,Head ~= V,Depth)
		->
		(
			remove_inconsistent_value(Key,Head,uniform(H),NewDistrH,Nremoved1,Depth),
			NewDistr=[V|NewDistrH],
			Nremoved is Nremoved1
		);
		(
			remove_inconsistent_value(Key,Head,uniform(H),NewDistrH,Nremoved1,Depth),
			NewDistr=NewDistrH,
			Nremoved is Nremoved1+1
		)
	).

/*
check_value(Value,Depth) :-
	recorded(sampled,Value,_),!.

check_value(H ~= V,Depth) :-
	user:evidence(H ~= V,1),!.
*/
check_value(K,H ~= V,Depth) :-
	% \+recorded(sampled,H ~= V,_),
	% \+user:evidence(H ~= V,_),
	recorda(K,H ~= V,Key),
	(
		evidence_proof_exists_maybe(K,Depth)
		->
		erase(Key);
		(
			erase(Key),
			fail
		)
	).
/*
TO CHECK
derived(Key,Head) :-
	ground(Head),
	recorded(Key,Head,_),
	!.
*/
derived(Key,Head) :-
	ground(Head),
	recorded(Key,Head,R),
	recorded(Key,H,R),
	H == Head,
	!.
	
% if Head is not grounded the check in "Key" is made without unification.
derived(Key,Head) :-
	\+ground(Head),
	copy_term(Head,Head2),
	recorded(Key,Head2,_),
	\+ \+ (numbervars(Head,0,_),numbervars(Head2,0,_),Head=Head2),
	%write(derived(Key,Head)),nl,
	!.

% compute the max priority present in the user model
get_max_priority(P) :-
	bb_put(priority,0),
	(
		
		(
			user:(hardclause(_,_,Pr)),
			bb_get(priority,PrOld),
			Pr > PrOld,
			bb_put(priority,Pr),
			fail;
			true
		),
		(
			user:(distributionalclause(_,_,_,Pr)),
			bb_get(priority,PrOld),
			Pr > PrOld,
			bb_put(priority,Pr),
			fail;
			true
		)
	),
	bb_delete(priority,P).
	


genesamplestep(Key,Weight,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		\+derived(Key,Head),% attention if the head is not grounded! TO FIX! only callmagic allowed (maybe)
		(
			Head\=callmagic(_) ->
				ground(Head)
			;
				true
		),
		recorda(Key,Head,_),
		% write(Head),nl,
		
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	bb_put(weightstep,1.0),
	(
		%between(0,Priority,Pr), % TO CHECK
		user:distributionalclause(Head2,Distribution,Body,Pr),
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), % TO CHECK!!!
		\+recorded(Key,Head2 ~= X,_),
		(/*
			Lookahead=false
			->*/
				(
					NewDistr=Distribution,
					WeightStep=1.0
				)
				%; sample_lookahead removed (old code, to check)
				%sample_lookahead(Key,Head2,Distribution,NewDistr,WeightStep) % write((Head2,Distribution,NewDistr,WeightStep)),nl,
		),
		sample(NewDistr,Val),
		% update weight
		bb_get(weightstep,OldWeight),
		NewWeight is OldWeight*WeightStep,
		bb_put(weightstep,NewWeight),
		
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' | '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			bb_delete(weightstep,W1),
			Flag=true,
			genesamplestep(Key,W,Pr,_),
			Weight is W1*W
		);
		(
			bb_delete(weightstep,Weight),
			Flag=false
		)
	).

genesamplestep_all(Key,Weight,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		\+recorded(Key,Head,_),% attention if the head is not grounded! TO FIX! only callmagic allowed (maybe)
		ground(Head),
		recorda(Key,Head,_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	bb_put(weightstep,1.0),
	(
		%between(0,Priority,Pr), % TO CHECK
		user:distributionalclause(Head2,Distribution,Body,Pr),
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), % TO CHECK!!!
		\+recorded(Key,Head2 ~= X,_),
		%sample_lookahead(Key,Head2,Distribution,NewDistr,WeightStep) % write((Head2,Distribution,NewDistr,WeightStep)),nl,
		finite(NewDistr)=Distribution,
		member(NewDistr,Val:WeightStep),
		% update weight
		bb_get(weightstep,OldWeight),
		NewWeight is OldWeight*WeightStep,
		bb_put(weightstep,NewWeight),
		
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' | '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			bb_delete(weightstep,W1),
			Flag=true,
			genesamplestep(Key,W,Pr,_),
			Weight is W1*W
		);
		(
			bb_delete(weightstep,Weight),
			Flag=false
		)
	).
/*
eraseall(Key) :-
	eraseall(Key).
*/
% generate sample handling priority
generate_sample_pr(Key,W,MaxP) :-
	bb_put(weight3,1.0),
	(
		between(0,MaxP,Priority),
		%write('P '),write(Priority),nl,
		genesamplestep(Key,Weight,Priority,F),

		bb_get(weight3,OldWeight1),
		NewWeight1 is OldWeight1*Weight,
		%write(NewWeight1),nl,
		bb_put(weight3,NewWeight1),
		fail;
	
		true
	),
	(
		bb_delete(weight3,W)
	).
	

check_neg_evidence(Key,[]).
check_pos_evidence(Key,[]).

check_neg_evidence(Key,[Neg|T]) :-
	%\+recorded(Key,Neg,_),
	query_proof(Key,\+Neg),
	check_neg_evidence(Key,T).

check_pos_evidence(Key,[Pos|T]) :-
	%recorded(Key,Pos,_),
	query_proof(Key,Pos),
	check_pos_evidence(Key,T).

check_evidence(Key,Pos,Neg) :-
	check_pos_evidence(Key,Pos),
	check_neg_evidence(Key,Neg).


% new inference algorithm
check_neg_evidence_exp(Key,List,[]).
check_pos_evidence_exp(Key,List,[]).

check_neg_evidence_exp(Key,List,[Neg|T]) :-
	query_proof(Key,\+Neg),
	\+memberchk(Neg,List),
	check_neg_evidence_exp(Key,List,T).

check_pos_evidence_exp(Key,List,[Pos|T]) :-
	%recorded(Key,Pos,_),
	query_proof(Key,Pos),
	check_pos_evidence_exp(Key,List,T).

check_pos_evidence_exp(Key,List,[Pos|T]) :-
	%recorded(Key,Pos,_),
	memberchk(Pos,List),
	check_pos_evidence_exp(Key,List,T).

check_evidence_exp(Key,List,Pos,Neg) :-
	check_pos_evidence_exp(Key,List,Pos),
	check_neg_evidence_exp(Key,List,Neg).
	
% evaluate the query generating N samples for timestep Start to End
eval_query_step(PosEvidence,NegEvidence,Query,Start,End,N,P,Succ_Sum,Sum) :-
	magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		eraseall(sampled),
		init_query(sampled,Query),
		init_query_list(sampled,PosEvidence),
		init_query_list(sampled,NegEvidence),
		bb_put(weight2,1.0),
		(
			between(Start,End,Timestep),
			retractall(user:timestep(_)),
			assert(user:timestep(Timestep)),
			%write('Timestep '),write(Timestep),nl,
			generate_sample_pr(sampled,Weight,MaxP),
			bb_get(weight2,OldWeight1),
			NewWeight1 is OldWeight1*Weight,
			%write(NewWeight1),nl,
			bb_put(weight2,NewWeight1),
						
			fail;
			
			true
		),
		check_evidence(sampled,PosEvidence,NegEvidence),
		bb_delete(weight2,NewWeight),
		%write(NewWeight),nl,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + NewWeight,
		bb_put(sample_sum,New_Sum),
		(
			query_proof(sampled,Query)
			->
			(
				bb_get(succeeding_sample_sum,Old),
				New is Old+NewWeight,
				bb_put(succeeding_sample_sum,New)
			);
			true
		),
		fail;

		true
	),
	eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

% evaluate the query generating N samples
eval_query(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	query(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum).

query(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	(
	inference(backward(INF)) ->
		(
			INF=lw ->
			eval_query_backward_lw(PosEvidence,NegEvidence,Query,1,N,P,Succ_Sum,Sum)
			;
			eval_query_backward_eval(PosEvidence,NegEvidence,Query,1,N,P,Succ_Sum,Sum)
		)
	;
		eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	),!.

query(PosEvidence,NegEvidence,Query,X,N,P,Succ_Sum,Sum) :-
	(
	inference(backward(INF)) ->
		(
			INF=lw ->
			eval_query_backward_lw(PosEvidence,NegEvidence,Query,X,N,P,Succ_Sum,Sum)
			;
			eval_query_backward_eval(PosEvidence,NegEvidence,Query,X,N,P,Succ_Sum,Sum)
		)
	;
		(writeln('forward not implemented'),fail) % not implemented eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	),!.

test_query(PosEvidence,NegEvidence,Query,X,N,P1) :-
	time(eval_query_backward(PosEvidence,NegEvidence,Query,X,N,P3,SQE3,S3)),
	writeln(backward(P3,SQE3,S3)),
	time(eval_query_backward_lw(PosEvidence,NegEvidence,Query,X,N,P1,SQE1,S1)),
	writeln(lw(P1,SQE1,S1)),
	time(eval_query_backward_lw_nolwquery(PosEvidence,NegEvidence,Query,X,N,P5,SQE5,S5)),
	writeln(lwnoquery(P5,SQE5,S5)),
	
	time(eval_query_backward_eval(PosEvidence,NegEvidence,Query,X,N,P2,SQE2,S2)),
	writeln(eval(P2,SQE2,S2)),
	time(eval_query_backward_noLW(PosEvidence,NegEvidence,Query,X,N,P4,SQE4,S4)),
	writeln(backwardnolw(P4,SQE4,S4)).

experiment_naive(File,Q,E,N,Repeat,AVG,S,T) :-
	I is cputime,
	findall(P,(between(1,Repeat,_),distributionalclause:eval_query_backward_noLW(E,[],Q,1,N,P1,_,SSum),(SSum>0 -> P=P1;P is random)),List),
	T is (cputime-I)/Repeat,
%	writeln(List),
	(ground(AVG)->
	variance(List,AVG,V)
	;
	avgvar(List,AVG,V)
	),
	S is sqrt(V),
	
	ConfVar is sqrt(2/Repeat*V*V)*2.575,
	ConfSInf is S-sqrt(max(0,V-ConfVar)),
	ConfSSup is sqrt(V+ConfVar)-S,
	ConfS is (ConfSSup+ConfSInf)/2,
	open(File,'append',F),
 	write(F,'naive MC'),
%	write('p('),write(Q),write('|'),write(E),write(')='),
	write(F,','),write(F,AVG),write(F,','),
 	write(F,N), write(F,','),write(F,T),write(F,','),
 	write(F,S),write(F,','),write(F,ConfSInf),write(F,','),write(F,ConfSSup),write(F,','),write(F,ConfS),nl(F),close(F).

experiment_LW(File,Q,E,N,Repeat,AVG,S,T) :-
	I is cputime,
	findall(P,(between(1,Repeat,_),distributionalclause:eval_query_backward_lw(E,[],Q,1,N,P1,_,SSum),(SSum>0 -> P=P1;P is random)),List),
	T is (cputime-I)/Repeat,
	(ground(AVG)->
	variance(List,AVG,V)
	;
	avgvar(List,AVG,V)
	),
	S is sqrt(V),
	ConfVar is sqrt(2/Repeat*V*V)*2.575, % 99% confidence interval
	ConfSInf is S-sqrt(max(0,V-ConfVar)),
	ConfSSup is sqrt(V+ConfVar)-S,
	ConfS is (ConfSSup+ConfSInf)/2,
	open(File,'append',F),
 	write(F,'LW'),%write('p('),write(Q),write('|'),write(E),write(')='),
	write(F,','),write(F,AVG),write(F,','),
 	write(F,N), write(F,','),write(F,T),write(F,','),
 	write(F,S),write(F,','),write(F,ConfSInf),write(F,','),write(F,ConfSSup),write(F,','),write(F,ConfS),nl(F),close(F).

experiment_LW2(File,Q,E,N,Repeat,AVG,S,T) :-
	I is cputime,
	append(Q,E,QE),
	N2 is round(N/2),
	N3 is round(N/2),
	findall(P,(between(1,Repeat,_),
	distributionalclause:eval_query_backward_lw([],[],QE,1,N2,PQE,_,_),
	distributionalclause:eval_query_backward_lw([],[],E,1,N3,PE,Succ_Sum,_),
	(Succ_Sum>0 -> P is PQE/PE;P is random)
	),List),
%	writeln(List),
	T is (cputime-I)/Repeat,
	(ground(AVG)->
	variance(List,AVG,V)
	;
	avgvar(List,AVG,V)
	),
	S is sqrt(V),
	ConfVar is sqrt(2/Repeat*V*V)*2.575,
	ConfSInf is S-sqrt(max(0,V-ConfVar)),
	ConfSSup is sqrt(V+ConfVar)-S,
	ConfS is (ConfSSup+ConfSInf)/2,
	open(File,'append',F),
 	write(F,'LW2'),%write('p('),write(Q),write('|'),write(E),write(')='),
	write(F,','),write(F,AVG),write(F,','),
 	write(F,N), write(F,','),write(F,T),write(F,','),
 	write(F,S),write(F,','),write(F,ConfSInf),write(F,','),write(F,ConfSSup),write(F,','),write(F,ConfS),nl(F),close(F).


experiment_LWnoquery(File,Q,E,N,Repeat,AVG,S,T) :-
	I is cputime,
	findall(P,(between(1,Repeat,_),distributionalclause:eval_query_backward_lw_nolwquery(E,[],Q,1,N,P1,_,SSum),(SSum>0 -> P=P1;P is random)),List),
	T is (cputime-I)/Repeat,
	(ground(AVG)->
	variance(List,AVG,V)
	;
	avgvar(List,AVG,V)
	),
	S is sqrt(V),
	ConfVar is sqrt(2/Repeat*V*V)*2.575,
	ConfSInf is S-sqrt(max(0,V-ConfVar)),
	ConfSSup is sqrt(V+ConfVar)-S,
	ConfS is (ConfSSup+ConfSInf)/2,
	open(File,'append',F),
 	write(F,'LW evidence'),%write('p('),write(Q),write('|'),write(E),write(')='),
	write(F,','),write(F,AVG),write(F,','),
 	write(F,N), write(F,','),write(F,T),write(F,','),
 	write(F,S),write(F,','),write(F,ConfSInf),write(F,','),write(F,ConfSSup),write(F,','),write(F,ConfS),nl(F),close(F).

experiment_LW_eval(File,Q,E,N,Repeat,AVG,S,T) :-
	I is cputime,
	findall(P,(between(1,Repeat,_),distributionalclause:eval_query_backward_eval(E,[],Q,1,N,P1,_,SSum),(SSum>0 -> P=P1;P is random)),List),
	T is (cputime-I)/Repeat,
	(ground(AVG)->
	variance(List,AVG,V)
	;
	avgvar(List,AVG,V)
	),
	S is sqrt(V),
	ConfVar is sqrt(2/Repeat*V*V)*2.575,
	ConfSInf is S-sqrt(max(0,V-ConfVar)),
	ConfSSup is sqrt(V+ConfVar)-S,
	ConfS is (ConfSSup+ConfSInf)/2,
	open(File,'append',F),
 	write(F,'experiment_eval'),%write('p('),write(Q),write('|'),write(E),write(')='),
	write(F,','),write(F,AVG),write(F,','),
 	write(F,N), write(F,','),write(F,T),write(F,','),
 	write(F,S),write(F,','),write(F,ConfSInf),write(F,','),write(F,ConfSSup),write(F,','),write(F,ConfS),nl(F),close(F).

experiment_LW_evalnoquery(File,Q,E,N,Repeat,AVG,S,T) :-
	I is cputime,
	findall(P,(between(1,Repeat,_),distributionalclause:eval_query_backward(E,[],Q,1,N,P1,_,SSum),(SSum>0 -> P=P1;P is random)),List),
	T is (cputime-I)/Repeat,
	(ground(AVG)->
	variance(List,AVG,V)
	;
	avgvar(List,AVG,V)
	),
	S is sqrt(V),
	ConfVar is sqrt(2/Repeat*V*V)*2.575,
	ConfSInf is S-sqrt(max(0,V-ConfVar)),
	ConfSSup is sqrt(V+ConfVar)-S,
	ConfS is (ConfSSup+ConfSInf)/2,
	open(File,'append',F),
 	write(F,'experiment_LW_evalnoquery'),%write('p('),write(Q),write('|'),write(E),write(')='),
	write(F,','),write(F,AVG),write(F,','),
 	write(F,N), write(F,','),write(F,T),write(F,','),
 	write(F,S),write(F,','),write(F,ConfSInf),write(F,','),write(F,ConfSSup),write(F,','),write(F,ConfS),nl(F),close(F).

% evaluate the query generating N samples
query2(ImpossibleEvidence,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	(
	inference(backward(_)) ->
		eval_query_backward2(ImpossibleEvidence,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	;
		fail %eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	),!.
	
% evaluate the query generating N samples
eval_query_exact(ExactVar,PosEvidence,NegEvidence,Query,N,FP) :-
	bb_put(exact,0.0),
	(
		user:distributionalclause(ExactVar,Distribution,Body,_),
		user:Body,
		ground(Distribution),
		ground(ExactVar),
		Distribution2=Distribution,
		(
			Distribution=uniform(D) ->
			(
				length(D,Size),
				W is 1/Size,
				member(Val,D),
				(
					inference(backward(_)) ->
						eval_query_backward([ExactVar~=Val|PosEvidence],NegEvidence,Query,1,N,P,Succ_Sum,Sum)
					;
						eval_query_forward([ExactVar~=Val|PosEvidence],NegEvidence,Query,N,P,Succ_Sum,Sum)
				),
				bb_get(exact,FinalP),
				FinalP2 is FinalP+P*W,
				write(ExactVar),write(' = '),write(Val),write(' W '),write(W),write(' P='),write(P),nl,
				bb_put(exact,FinalP2),
				fail;
				true
			)
			;
			(
				Distribution2=finite(D2),
				member(W:Val,D2),
				(
					inference(backward(_)) ->
						eval_query_backward([ExactVar~=Val|PosEvidence],NegEvidence,Query,1,N,P,Succ_Sum,Sum)
					;
						eval_query_forward([ExactVar~=Val|PosEvidence],NegEvidence,Query,N,P,Succ_Sum,Sum)
				),
				bb_get(exact,FinalP),
				FinalP2 is FinalP+P*W,
				bb_put(exact,FinalP2),
				fail;
				true
			)
		)
	),
	bb_delete(exact,FP),
	!.


	
% evaluate the query generating N samples using forward reasoning
eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		eraseall(sampled),
		init_query(sampled,Query),
		init_query_list(sampled,PosEvidence),
		init_query_list(sampled,NegEvidence),
		generate_sample_pr(sampled,NewWeight,MaxP),
		check_evidence(sampled,PosEvidence,NegEvidence),
		% write(NewWeight),nl,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + NewWeight,
		bb_put(sample_sum,New_Sum),
		(
			query_proof(sampled,Query)
			->
			(
				bb_get(succeeding_sample_sum,Old),
				New is Old+NewWeight,
				bb_put(succeeding_sample_sum,New)
			);
		  	true
		),
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		),
		fail;

		true
	),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

printsample(V) :-
	findall(A,(recorded(V,A,_), write(A),nl),_).

% evaluate the query generating N samples with backward reasoning NOT COMPLETE
eval_query_backward2(ImpossibleEvidence,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	(get_debug(filecsv)->
		(open('debug.txt','write',S11),close(S11))
	;
		true
	),
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	test_to_list(PosEvidenceTuple,PosEvidence),
	(
		between(1,N,I),
		eraseall(sampled),
		abolish_all_tables,
		assert_impossible_evidence(sampled,ImpossibleEvidence),
		
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------')
			;
			(
			get_debug(filecsv)->
				(
				open('debug.txt','append',S),
				forall(recorded(sampled,current(A)~=ValueA,_),(
				write(S,'current,'),
				A=.. [Name|Args],
				write(S,Name),
				write(S,','),
				length(Args,ArityArgs),
				write(S,ArityArgs),
				write(S,','),
				forall(member(AR,Args),(write(S,AR),write(S,','))),
				test_to_list(ValueA,ListA),
				length(ListA,ArityValueA),
				write(S,ArityValueA),
				write(S,','),
				forall(member(AR,ListA),(write(S,AR),write(S,','))),
				nl(S) )),
				
				forall(recorded(sampled,next(A)~=ValueA,_),(
				write(S,'next,'),
				A=.. [Name|Args],
				write(S,Name),
				write(S,','),
				length(Args,ArityArgs),
				write(S,ArityArgs),
				write(S,','),
				forall(member(AR,Args),(write(S,AR),write(S,','))),
				test_to_list(ValueA,ListA),
				length(ListA,ArityValueA),
				write(S,ArityValueA),
				write(S,','),
				forall(member(AR,ListA),(write(S,AR),write(S,','))),
				nl(S) )),
				
				forall(recorded(sampled,action(A),_),(
				write(S,'action,'),
				A=.. [Name|Args],
				write(S,Name),
				write(S,','),
				%length(Args,ArityArgs),
				write(S,1),
				write(S,','),
				[FirstArg|OtherArgs]=Args,
				write(S,FirstArg),write(S,','),
				length(OtherArgs,ArityOtherArgs),
				write(S,ArityOtherArgs),
				write(S,','),
				forall(member(AR,OtherArgs),(write(S,AR),write(S,','))),
				nl(S) )),
				nl(S),
				close(S)
				)
				;
				true
			
			)
		),
		fail;

		true
	),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.	
% evaluate the query generating N samples with backward reasoning
eval_query_backward(PosEvidence,NegEvidence,Query1,X,N,P,Succ_Sum,Sum) :-
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	(is_list(Query1)->
	(test_to_list(Query,Query1),ListQuery=Query1)
	;
	(test_to_list(Query1,ListQuery),Query1=Query)
	),
	test_to_list(PosEvidenceTuple,PosEvidence),
	forall( between(1,N,I),
	((
		eraseall(sampled),
		abolish_all_tables,
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%	check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2*X,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true)
	),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

% evaluate the query generating N samples with backward reasoning
eval_query_backward_noLW(PosEvidence,NegEvidence,Query1,X,N,P,Succ_Sum,Sum) :-
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
%	retractall(user:evidence(_,_)),
%	assert_evidence(PosEvidence,1),
%	assert_evidence(NegEvidence,0),
	(is_list(Query1)->
	(test_to_list(Query,Query1),ListQuery=Query1)
	;
	(test_to_list(Query1,ListQuery),Query1=Query)
	),
	test_to_list(PosEvidence1,PosEvidence),
	forall( between(1,N,I),
	((
		eraseall(sampled),
		abolish_all_tables,
		%check_evidence_backward(sampled,PosEvidence,W1),
		%
		proof_query_backward(sampled,PosEvidence1),
		check_evidence(sampled,PosEvidence,NegEvidence),
		W1=1.0,
%		W1>0,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + 1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2*X,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true)
	),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.


% evaluate the distribution of X in a query
% sum_distrib is SLOW!
eval_query_distribution(X,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	test_to_list(PosEvidenceTuple,PosEvidence),
	forall( between(1,N,I),
	((
		eraseall(sampled),
		abolish_all_tables,
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			
			findall(1:X,proof_query_backward(sampled,Query),List)
			->
			(
				
				bb_get(succeeding_sample_sum,Old),
				sum_distrib(finite(Old),finite(List),W1,finite(New)),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
			
			/*		  	
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true*/
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true)
	),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	(
		Succ_Sum==[] ->
		P=[]
		;
		divideby(Succ_Sum,Sum,P)
	),
	retractall(user:evidence(_,_)),
	!.

% evaluate the distribution not compact of X in a query, not normalized
eval_query_valuelist(X,PosEvidence,NegEvidence,Query,N,P,Sum) :-
	bb_put(sample_sum,0.0),
%	bb_put(succeeding_sample_sum,[]),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	test_to_list(PosEvidenceTuple,PosEvidence),
	findall(W1:X,( between(1,N,I),
		eraseall(sampled),
		abolish_all_tables,
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		proof_query_backward(sampled,Query),/*
		(
			
			findall(W1:X,proof_query_backward(sampled,Query),List)
			->
			(
				true
			%	bb_get(succeeding_sample_sum,Old),
				%sum_distrib(finite(Old),finite(List),W1,finite(New)),
			%	append([Old,List],New),
				%write((List,Weight,New)),nl,
			%	bb_put(succeeding_sample_sum,New)
			)
			;
		  	List=[]
		),	*/	  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	),P),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
%	bb_delete(succeeding_sample_sum,Succ_Sum),
	%P=List,
	retractall(user:evidence(_,_)),
	!.
	
% evaluate the distribution not compact of X in a query
eval_query_valuelist2(X,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
%	bb_put(sample_sum,0.0),
%	bb_put(succeeding_sample_sum,[]),
	initmap(Map),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	test_to_list(PosEvidenceTuple,PosEvidence),
	forall( between(1,N,I),
	((
		eraseall(sampled),
		abolish_all_tables,
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		%bb_get(sample_sum,Old_Sum),
		%New_Sum is Old_Sum + W1,
		%bb_put(sample_sum,New_Sum),
		(
			forall(proof_query_backward(sampled,Query),addvaluemap(Map,X,W1))
			/*findall(W1:X,proof_query_backward(sampled,Query),List)
			->
			(
				
				bb_get(succeeding_sample_sum,Old),
				%sum_distrib(finite(Old),finite(List),W1,finite(New)),
				append([Old,List],New),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
*/
			
			/*		  	
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true*/
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true)
	),
	%eraseall(sampled),
	%bb_delete(sample_sum,Sum),
	%bb_delete(succeeding_sample_sum,Succ_Sum),
	%P=Succ_Sum,
	writeln(ok),
	averagemap(Map,P),
	retractall(user:evidence(_,_)),
	!.

% evaluate the distribution of X in a query Experimental! not finished
eval_query_distribution_eval(X,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	test_to_list(PosEvidenceTuple,PosEvidence),
	forall( between(1,N,I),
	((
		eraseall(sampled),
		eraseall(sampledtemp),
		abolish_all_tables,
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			% todo: create a proof_query_backward_eval that returns a distribution P(X|...) to add to the previous.
			findall(W2:X,(proof_query_backward_eval(sampled,sampledtemp,Query,W2),writeln(W2:X)),List)
			->
			(
				%writeln(List),
				bb_get(succeeding_sample_sum,Old),
				%writeln(Old),
				sum_distrib(finite(Old),finite(List),W1,finite(New)),
				%writeln(New),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
			
			/*		  	
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true*/
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true)
	),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	(
		Succ_Sum==[] ->
		P=[]
		;
		divideby(Succ_Sum,Sum,P)
	),
	retractall(user:evidence(_,_)),
	!.

generate_backward(Query,L) :-
	eraseall(sampled),
	abolish_all_tables,
	(proof_query_backward(sampled,Query);true),
	findall(A,recorded(sampled,A,_),L),!.

generate_forward(L) :-
	eraseall(sampled),
	abolish_all_tables,
	get_max_priority(MaxP),
	generate_sample_pr(sampled,NewWeight,MaxP),
	findall(A,recorded(sampled,A,_),L),!.

% to check
eval_query_backward_eval(PosEvidence,NegEvidence,Query1,X,N,P,Succ_Sum,Sum) :-
	%magic,
%	get_max_priority(MaxP),
	(is_list(Query1)->
	(test_to_list(Query,Query1),ListQuery=Query1)
	;
	(test_to_list(Query1,ListQuery),Query1=Query)
	),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	assert_evidence(Query,query),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	test_to_list(PosEvidenceTuple,PosEvidence),
	forall(	between(1,N,I),(
	(
		eraseall(sampled),
%		eraseall(sampledtemp),
		abolish_all_tables,
		bb_put(queryevidence,1),
		proof_query_backward_eval(sampled,PosEvidenceTuple,W1),%check_evidence_backward(sampled,PosEvidence,W1),
		W1>0,
		check_evidence(sampled,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		bb_put(queryevidence,query),
		(
			(proof_query_backward_eval(sampled,Query,W2),%check_evidence_backward(sampled,[Query],W2),
			check_evidence(sampled,ListQuery,[])
			) %
			%(bb_put(wevidence,1.0),proof_query_backward_lw(sampled,Query,Query),bb_delete(wevidence,W2))
			%proof_query_backward_eval(sampled,sampledtemp,Query,W2)
			->
			(
				%W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				%check_evidence(sampled,[Query],[]),
				bb_get(succeeding_sample_sum,Old),
				New is Old+X*W1*W2,
				%writeln(W2),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)/*,
		
		fail;

		true*/
	);true) ),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

groundedquery([]) :- !.

groundedquery([Q|T]) :-
	(ground(Q);(Q= H~=V,ground(H))),
	groundedquery(T).

onlygrounded([],[]) :- !.

onlygrounded([Q|T],GroundedQuery) :-
	(
	ground(Q)->
		(
		onlygrounded(T,GroundedQueryT),
		GroundedQuery=[Q|GroundedQueryT]
		)
	;
		(
		onlygrounded(T,GroundedQuery)
		)
	).
	
	
headgrounded([],[]) :- !.

headgrounded([Q|T],GroundedQuery) :-
	(
	(ground(Q);(Q= H~=V,ground(H)))->
		(
		headgrounded(T,GroundedQueryT),
		GroundedQuery=[Q|GroundedQueryT]
		)
	;
		headgrounded(T,GroundedQuery)
	).


% to finish
expandquery([],[]) :-!.
expandquery([A|T],NewE) :-

	expandquery(T,ExpT),
	(A = H ~= V ->
	(
		findall((H,Distribution,Body),user:distributionalclause(H,Distribution,Body,_),[(HH,DD,BB)]) ->
		(
			test_to_list(BB,BBList),
			HH=H,
			(DD=val(XX) -> XX=V;true),
			(DD=finite([1:XX]) -> XX=V;true),
			(DD=uniform([XX]) -> XX=V;true),
			append([HH ~= V],BBList,AB),
			
			append(AB,ExpT,NewE)
		)
		;
		NewE=[A|ExpT]
	)
	;
	(
		findall((A,Body),user:hardclause(A,Body,_),[(AA,BB)]) ->
		(
			test_to_list(BB,BBList),
			A=AA,
			append(AA,BBList,AB),
			append(AB,ExpT,NewE)
		)
		;
		NewE=[ExpT] % we remove facts because we cannot ground % old code: NewE=[A|ExpT]
	)
	).


main_evalquery_lw(NPE,NQQ,X,I,PosEvidence,NegEvidence,InitialPosEvidence,PosEvidenceTuple,Query,InitialQuery) :-
	
	(
	eraseall(I),
	abolish_all_tables,
	
	(
	PosEvidence\=[]->
	(
		bb_put(q,InitialPosEvidence),
		%bb_put(nq,NPE),
		bb_put(wevidence,1.0),
		bb_put(dx,0),
		% GPosEvidence has to be ground in variable name
			proof_query_backward_lw(I,PosEvidenceTuple),
			bb_delete(wevidence,W1),
			bb_delete(dx,DX)
	)
	;
		(
		DX=0,
		W1=1
		)
	),
	W1>0,
	bb_get(dxe,DXE),
	DXE>=DX,
	
	(DXE>DX ->
		(
		check_evidence(I,PosEvidence,NegEvidence),
		bb_put(sample_sum,W1),
		%bb_put(succeeding_sample_sum,0),
		bb_put(dxe,DX)
		)
		;
		(
		check_evidence(I,PosEvidence,NegEvidence),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum)
			
		)
	),
/*	check_evidence(I,PosEvidence,NegEvidence),
	
	bb_get(sample_sum,Old_Sum),
	New_Sum is Old_Sum + W1,
	bb_put(sample_sum,New_Sum),*/
	
	bb_put(q,InitialQuery),
%	bb_put(nq,NQQ),
	
	(
		(bb_put(wevidence,1.0),bb_put(dx,0),
		proof_query_backward_lw(I,Query),
		bb_delete(wevidence,W2))
		->
		(
			
			bb_get(dxq,DXEQ),
			bb_get(dx,DX2),
			DXEQ>=DX+DX2,
			(DXEQ>DX+DX2 ->
				(
				NW is X*W1*W2,
				bb_put(succeeding_sample_sum,NW),
				NEWDXEQ is DX+DX2,
				bb_put(dxq,NEWDXEQ)
				%writeln((Query,NW))
				)
				;
				(
				bb_get(succeeding_sample_sum,Old),
				New is Old+X*W1*W2,
				bb_put(succeeding_sample_sum,New)
				)
			)
			
		)
		;
	  	true %(write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------'))%true
	),		  	
	(
	get_debug(true)->
		write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(I,A,_), write(A),nl),_),nl,writeln('------------------------------------');
		true
	)
	),!.

% var needs to be grounded
eval_query_backward_lw(PosEvidence,NegEvidence,Query1,X,N,P,Succ_Sum,Sum) :-
	%magic,
%	get_max_priority(MaxP),
%	retractall(user:evidence(_,_)),
%	assert_evidence(PosEvidence,1),
%	assert_evidence(NegEvidence,0),
%	assert_evidence(Query,query),
	(is_list(Query1)->
	(test_to_list(Query,Query1),ListQuery=Query1)
	;
	(test_to_list(Query1,ListQuery),Query1=Query)
	),
	
%	expandquery(ListQuery,InitialQueryExpanded),
	InitialQueryExpanded=ListQuery,
%	writeln(ListQuery),
	%InitialQuery=..[q|InitialQueryExpanded],
	InitialQuery=InitialQueryExpanded,
%	writeln(InitialQuery),
%	duplicate_term(PosEvidence,GPosEvidence),
%	duplicate_term(ListQuery,GQuery),
	
%	writeln(GPosEvidence),
%	writeln(GQuery),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(dxe,10000), % should be infinite
	bb_put(dxq,10000), % should be infinite
%	eraseall(proposal),
%	user:proposals,
	I=sampledtemp,%I,
	test_to_list(PosEvidenceTuple,PosEvidence),
%	expandquery(PosEvidence,InitialPosEvidenceExpanded),
	InitialPosEvidenceExpanded=PosEvidence,
	InitialPosEvidence=PosEvidence,
	%InitialPosEvidence=..[q|InitialPosEvidenceExpanded],
%	writeln(InitialPosEvidenceExpanded),
%	length(InitialQueryExpanded,NQQ),
%	length(InitialPosEvidenceExpanded,NPE),
	
	(between(1,N,_),
	main_evalquery_lw(NPE,NQQ,X,I,PosEvidence,NegEvidence,InitialPosEvidence,PosEvidenceTuple,Query,InitialQuery),
	/*(
		
		%recorda(proposal,proposalprob(I,1.0),_),
		eraseall(I),
%		eraseall(sampledtemp),
		abolish_all_tables,
%		bb_put(queryevidence,1),
%		check_evidence_backward(sampled,PosEvidence,W1),
		
		(
		PosEvidence\=[]->
		(
			bb_put(q,InitialPosEvidence),
			bb_put(wevidence,1.0),
			
			% GPosEvidence has to be ground in variable name
			proof_query_backward_lw(I,PosEvidenceTuple),
			bb_delete(wevidence,W1)
		)
		;
			W1=1
		),
		W1>0,
		check_evidence(I,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		bb_put(q,InitialQuery),
%		bb_put(queryevidence,query),
		(
			%(check_evidence_backward(sampled,[Query],W2),check_evidence(sampled,[Query],[])) %
			(bb_put(wevidence,1.0),
			proof_query_backward_lw(I,Query),
			bb_delete(wevidence,W2)) %,check_evidence(sampled,ListQuery,[]))
			%proof_query_backward_eval(sampled,sampledtemp,Query,W2)
			->
			(
				%W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				%(check_evidence(sampled,Query,[])->true;writeln('error')),
				bb_get(succeeding_sample_sum,Old),
				New is Old+X*W1*W2,
				%writeln(W2),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true %(write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------'))%true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(I,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	),*/
	fail;true),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
%	retractall(user:evidence(_,_)),
	!.

eval_query_backward_lw_nolwquery(PosEvidence,NegEvidence,Query1,X,N,P,Succ_Sum,Sum) :-
	writeln('oldcode'),halt,
	(is_list(Query1)->
	(test_to_list(Query,Query1),ListQuery=Query1)
	;
	(test_to_list(Query1,ListQuery),Query1=Query)
	),
%	duplicate_term(PosEvidence,GPosEvidence),
%	duplicate_term(ListQuery,GQuery),
%	writeln(GPosEvidence),
%	writeln(GQuery),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
%	eraseall(proposal),
%	user:proposals,
	I=sampledtemp,%I,
	test_to_list(PosEvidenceTuple,PosEvidence),
	forall(	between(1,N,_),(
	(
		
		%recorda(proposal,proposalprob(I,1.0),_),
		eraseall(I),
%		eraseall(sampledtemp),
		abolish_all_tables,
%		bb_put(queryevidence,1),
%		check_evidence_backward(sampled,PosEvidence,W1),
		(
		PosEvidence\=[]->
		(
			bb_put(q,PosEvidence),
			bb_put(wevidence,1.0),
			
			% GPosEvidence has to be ground in variable name
			proof_query_backward_lw(I,PosEvidenceTuple),
			bb_delete(wevidence,W1)
		)
		;
			W1=1
		),
		W1>0,
		check_evidence(I,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),

		(
			%(check_evidence_backward(sampled,[Query],W2),check_evidence(sampled,[Query],[])) %
			(proof_query_backward(I,Query)) %,check_evidence(sampled,ListQuery,[]))
			%proof_query_backward_eval(sampled,sampledtemp,Query,W2)
			->
			(
				W2=1.0,
				bb_get(succeeding_sample_sum,Old),
				New is Old+X*W1*W2,
				%writeln(W2),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true %(write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------'))%true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(I,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true) ),
	%eraseall(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
%	retractall(user:evidence(_,_)),
	!.

defaultproposal(Head,Body,finite,finite(Distr),NewDistr) :-
	writeln(defaultproposal(Head,Body,finite,finite(Distr),NewDistr)),
	length(Distr,N),P is 1/N,
	findall(P:V,member(_:V,Distr),NewDistr),
	user:distributionalclause(Head,finite(Distr),Body2,_),
	duplicate_term(Body2,Body1),
	Body=Body1,
	recorda(proposal,localproposal(Head,finite(Distr),Body2,finite,NewDistr),_),
	writeln(recorda(proposal,localproposal(Head,finite(Distr),Body2,finite,NewDistr),_)),
	!.
% var needs to be grounded
eval_query_backward_lw_adapt(PosEvidence,NegEvidence,Query,N,PNew,Succ_Sum,Sum,OldP,LearnRate) :-

	test_to_list(Query,ListQuery),

	duplicate_term(PosEvidence,GPosEvidence),
	duplicate_term(ListQuery,GQuery),
%	writeln(GPosEvidence),
%	writeln(GQuery),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(recorded(proposal,proposalprob(_,_),Ref),erase(Ref),fail;true),
	(recorded(proposal,wi(_,_),Ref),erase(Ref),fail;true),
	(recorded(proposal,relderivative(_,_,_,_,_,_),Ref),erase(Ref),fail;true),
	forall(	between(1,N,I),(
	(
		recorda(proposal,proposalprob(I,1.0),_),
		eraseall(I),
%		eraseall(sampledtemp),
		
		abolish_all_tables,
		
%		bb_put(queryevidence,1),
%		check_evidence_backward(sampled,PosEvidence,W1),
		(
		PosEvidence\=[]->
		(
			bb_put(wevidence,1.0),
			test_to_list(PosEvidence1,PosEvidence),
			% GPosEvidence has to be ground in variable name
			proof_query_backward_lw_adapt(I,PosEvidence1,GPosEvidence),
			bb_delete(wevidence,W1)
		)
		;
			W1=1
		),
		W1>0,
		check_evidence(I,PosEvidence,NegEvidence),
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),

%		bb_put(queryevidence,query),
		(
			%(check_evidence_backward(sampled,[Query],W2),check_evidence(sampled,[Query],[])) %
			(bb_put(wevidence,1.0),
			proof_query_backward_lw_adapt(I,Query,GQuery),
			bb_delete(wevidence,W2)) %,check_evidence(sampled,ListQuery,[]))
			%proof_query_backward_eval(sampled,sampledtemp,Query,W2)
			->
			(
				
				%W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				%(check_evidence(sampled,Query,[])->true;writeln('error')),
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(W2),
				bb_put(succeeding_sample_sum,New),
				Wi is W1*W2,
				recorda(proposal,wi(I,Wi),_)
			)
			;
		  	recorda(proposal,wi(I,0),_) %(write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------'))%true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(I,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		)
	);true) ),
	%eraseall(sampled),
	
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	
%	retractall(user:evidence(_,_)),
%	P1 is (Succ_Sum+1/10^300)/(Sum+5/10^300),
	PNew is OldP*(1-LearnRate)+LearnRate*P,
	writeln(PNew is OldP*(1-LearnRate)+LearnRate*P),
	Diff is abs(P-OldP)/(P+OldP)*2*100,
	write('diff% : '),writeln(Diff),
	computenewproposal(N,PNew,5),
	!.
% finite distribution
computenewproposal(N,G,Rate) :-
	forall((
		user:adapt(Head),
		recorded(proposal,localproposal(Head,OrigDistr,Body,TypeD,PropD),OldR)),
	(
		
		TypeD==finite,
		length(PropD,Dim),
		findall(NewP:Val,
		(
			member(OldP:Val,PropD),
			bb_put(tmp,0.0),
			forall(between(1,N,I),
			(
				recorded(proposal,proposalprob(I,ProbPi),_),
				recorded(proposal,wi(I,Wi),_),
				%writeln(proposalprob(I,ProbPi)),
				%writeln(wi(I,Wi)),
				%writeln(localproposal(Head,OrigDistr,Body,TypeD,PropD)),
				(recorded(proposal,relderivative(I,Head,OrigDistr,Body,Val,RelDer),_) -> true;RelDer=0),
				
				bb_get(tmp,OldD),
				NewD is OldD+RelDer*ProbPi*(1-Wi/G),
				bb_put(tmp,NewD)
			)),
			bb_delete(tmp,Deriv),
			NDer is Deriv/N,
			
			NewP is min(max(0.001/Dim,OldP-Rate*NDer),10000/Dim)
		),List),
		normalize(List,NormDistr),
		erase(OldR),
		recorda(proposal,localproposal(Head,OrigDistr,Body,TypeD,NormDistr),_),
		writeln((Head,NormDistr))
	)).
	


get_counts(L,Sum) :-
	recorded(ris,count(Value,Count),K),
	erase(K),
	P is Count/Sum,
	L=[(Value,P)|H],
	get_counts(H,Sum).

get_counts([],Sum) :-
	\+recorded(ris,count(Value,Count),K),
	!.
	
print_hardclause :-
	user:hardclause(_H,_B,_Pr),
	numbervars(user:hardclause(_H,_B,_Pr),1,_),
	write(_H),
	write(' :- '),
	write(_B),
	write(' Pr '),
	write(_Pr),
	nl,fail;true.

print_distributionalclause :-
	user:distributionalclause(_H,_D,_B,_Pr),
	numbervars(user:distributionalclause(_H,_D,_B,_Pr),1,_),
	write(_H),
	write(' ~ '),
	write(_D),
	write(' :- '),
	write(_B),
	write(' Pr '),
	write(_Pr),
	nl,fail;true.

print_distributionalclause_global :-
	recorded(global,distributionalclause(_H,_D,_B,_Pr),_),
	numbervars(user:distributionalclause(_H,_D,_B,_Pr),1,_),
	write(_H),
	write(' ~ '),
	write(_D),
	write(' :- '),
	write(_B),
	write(' Pr '),
	write(_Pr),
	nl,fail;true.

print_all :-
	print_hardclause,
	print_distributionalclause.
	
init :-
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	eraseall(sampled),
	magic.
	%write('init'),nl.

	
%%%%%%%%% private part %%%%%%%%%
proof_query_backward_lazy(Key,true) :-
	!.
	
proof_query_backward_lazy(Key,(A,B)) :-
	!,
	proof_query_backward_lazy(Key,A),
	proof_query_backward_lazy(Key,B).

	
% TO CHECK
proof_query_backward_lazy(Key,findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward_lazy(Key,Y),Z),
	!.



proof_query_backward_lazy(Key,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lazy(Key,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward_lazy(Key,\+A) :-
	\+proof_query_backward_lazy(Key,A),!.	

proof_query_backward_lazy(Key,A) :-
	ground(A),
%	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.
	
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward_lazy(Key,H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val,
	!.
	
proof_query_backward_lazy(Key,A) :-
	recorded(Key,A,_),
	A\= _~= distribution(_).


% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward_lazy(Key,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	(
	(\+erased(R),recorded(Key,H ~= V,_)) -> % TO TEST
		(
			V=S,
			writeln('warning '),
			writeln(recorded(Key,H ~= distribution(D),R)),
			writeln( proof_query_backward_lazy(Key,H ~= S)),
			dcpf:printkeyp(Key),nl,
			erase(R)
		)
		;
		(
		recorda(Key,H ~= Val,_),
		S=Val
		)
	
	).
	

%%% Tabling %%%
proof_query_backward_lazy(Key,Head ~= Val) :-
	tabling_proof_query_backward_lazy(Key,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Key,Head ~= Var,_),
	Var=Val.

proof_query_backward_lazy(Key,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_lazy(Key,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

%proof_query_backward_lazy(Key,A) :-
%	recorded(Key,A,_).

tabling_proof_query_backward_lazy(Key,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward_lazy(Key,Body).

	
tabling_proof_query_backward_lazy(Key,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_lazy(Key,Body).


proof_query_backward_lazy(Key,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_lazy(Key,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Key,Head ~= Var,_),
	Var=Val.



% with a temporary index to store sampled variables
% don't use Key=Temp!

proof_query_backward_lazy(Key,Key,_) :-
	!,
	writeln('error proof_query_backward_lazy: Key=Temp'),
	!.

proof_query_backward_lazy(Key,Temp,true) :-
	!.
	
proof_query_backward_lazy(Key,Temp,(A,B)) :-
	!,
	proof_query_backward_lazy(Key,Temp,A),
	proof_query_backward_lazy(Key,Temp,B).


% Really slow!
proof_query_backward_lazy(Key,Temp,findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward_lazy(Key,Temp,Y),Z),
	!.

proof_query_backward_lazy(Key,Temp,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lazy(Key,Temp,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward_lazy(Key,Temp,\+A) :-
	\+proof_query_backward_lazy(Key,Temp,A),!.

proof_query_backward_lazy(Key,Temp,A) :-
	ground(A),
%	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.

proof_query_backward_lazy(Key,Temp,A) :-
	ground(A),
%	A\= _~= distribution(_),
	recorded(Temp,A,_),
	!.
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward_lazy(Key,Temp,H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	%\+recorded(Temp,H ~= _,_), % is always true
	sample(D,Val),
%	erase(R),
	recorda(Temp,H ~= Val,_),
	S=Val,
	!.
%%% Tabling %%%

proof_query_backward_lazy(Key,Temp,A) :-
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_lazy(Key,Temp,A) :-
	recorded(Temp,A,_),
	A\= _~= distribution(_).

% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward_lazy(Key,Temp,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	recorda(Temp,H ~= Val,_),
	S=Val.

proof_query_backward_lazy(Key,Temp,Head ~= Val) :-
	tabling_proof_query_backward_lazy2(Key,Temp,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_lazy(Key,Temp,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_lazy(Key,Temp,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_lazy(Key,Temp,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_lazy2(Key,Temp,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).
	
tabling_proof_query_backward_lazy2(Key,Temp,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward_lazy(Key,Temp,Body).
	
tabling_proof_query_backward_lazy2(Key,Temp,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_lazy(Key,Temp,Body).
	


% don't use Key=Temp
proof_query_backward_lazy_eval(Key,Temp,true,1.0) :-
	!.

% error in proof_query_backward_lazy_eval	
proof_query_backward_lazy_eval(Key,Temp,(A,B),W) :-
	!,
	proof_query_backward_lazy_eval(Key,Temp,A,W1),
	proof_query_backward_lazy_eval(Key,Temp,B,W2),
	W is W1*W2.

% TO CHECK
proof_query_backward_lazy_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
	findall(X,proof_query_backward_lazy(Key,Temp,Y),Z),
	!.

proof_query_backward_lazy_eval(Key,Temp,\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lazy_eval(Key,Temp,A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_lazy_eval(Key,Temp,\+A,1.0) :-
	\+proof_query_backward_lazy(Key,Temp,A),!.


proof_query_backward_lazy_eval(Key,Temp,A,1.0) :-
	ground(A),
	A\=(\+_),
	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.

proof_query_backward_lazy_eval(Key,Temp,A,1.0) :-
	ground(A),
	A\=(\+_),
	A\= _~= distribution(_),
	recorded(Temp,A,_),
	!.
	
% to support non-sampled variables H ~= distribution(D) in the particles	
proof_query_backward_lazy_eval(Key,Temp,H~=Val,W) :-
	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	likelihood_weighting(Val,D,W),
	recorda(Temp,H~=Val,_),
	!.

%%% Tabling %%%

proof_query_backward_lazy_eval(Key,Temp,A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_lazy_eval(Key,Temp,A,1.0) :-
	A\=(\+_),
	recorded(Temp,A,_),
	A\= _~= distribution(_).

proof_query_backward_lazy_eval(Key,Temp,H ~= S,1.0) :-
	\+ground(S),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	recorda(Temp,H ~= Val,_),
	S=Val.
	
proof_query_backward_lazy_eval(Key,Temp,Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward_lazy(Key,Temp,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_lazy_eval(Key,Temp,Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_lazy(Key,Temp,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_lazy_eval(Key,Temp,Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_lazy(Key,Temp,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).



% check evidence eval
proof_query_backward_lazy_eval(Key,true,1.0) :-
	!.

proof_query_backward_lazy_eval(Key,(A,B),W) :-
	!,
	proof_query_backward_lazy_eval(Key,A,W1),
	proof_query_backward_lazy_eval(Key,B,W2),
	W is W1*W2.

% TO CHECK
proof_query_backward_lazy_eval(Key,findall_forward(X,Y,Z),1.0) :-
	findall(X,proof_query_backward_lazy(Key,Y),Z),
	!.

proof_query_backward_lazy_eval(Key,\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_lazy_eval(Key,A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_lazy_eval(Key,\+A,1.0) :-
	\+proof_query_backward_lazy(Key,A),!.

proof_query_backward_lazy_eval(Key,A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_),
	!.

% to support non-sampled variables H ~= distribution(D) in the particles		
proof_query_backward_lazy_eval(Key,H~=Val,W) :-
	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	likelihood_weighting(Val,D,W),
	erase(R),
	recorda(Key,H~=Val,_),
	!.
	
%%% Tabling %%%

proof_query_backward_lazy_eval(Key,A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_).

proof_query_backward_lazy_eval(Key,H ~= S,1.0) :-
	\+ground(S),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val.
		
proof_query_backward_lazy_eval(Key,Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward_lazy(Key,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Key,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Key,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_lazy_eval(Key,Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_lazy(Key,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Key,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Key,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_lazy_eval(Key,Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_lazy(Key,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).



query_proof_rao(Key,true,1.0) :-
	!.
query_proof_rao(Key,(A,B),W) :-
	!,
	query_proof_rao(Key,A,W1),
	query_proof_rao(Key,B,W2),
	W is W1*W2.


query_proof_rao(Key,current(A) ~= Val,W) :-
	user:rao(A),
	recorded(Key,current(A) ~= finite(Distribution),_),
	member(W:Val,Distribution).
	
query_proof_rao(Key,next(A) ~= Val,W) :-
	user:rao(A),
	recorded(Key,next(A) ~= finite(Distribution),_),
	member(W:Val,Distribution).
	%findall(W:Val,member(W:Val,Distribution),List).
	
query_proof_rao(Key,current(A) ~= Val,0.0) :-
	user:rao(A),
	recorded(Key,current(A) ~= finite(Distribution),_),
	\+member(W:Val,Distribution).
	
query_proof_rao(Key,next(A) ~= Val,0.0) :-
	user:rao(A),
	recorded(Key,next(A) ~= finite(Distribution),_),
	\+member(W:Val,Distribution).

query_proof_rao(Key,current(A) ~= Val,1.0) :-
	\+user:rao(A),
	(
		inference(backward(_)) ->
			proof_query_backward(Key,current(A) ~= Val)
		;
			query_proof(Key,current(A) ~= Val)
	).
	
query_proof_rao(Key,next(A) ~= Val,1.0) :-
	\+user:rao(A),
	(
		inference(backward(_)) ->
			proof_query_backward(Key,next(A) ~= Val)
		;
			query_proof(Key,next(A) ~= Val)
	).
	
query_proof_rao(Key,A,1.0) :-
	A\= _ ~= _,
	A\= (\+ _),
	(
		inference(backward(_)) ->
			proof_query_backward(Key,A)
		;
			query_proof(Key,A)
	).

%TO TEST

query_proof_rao(Key,\+A,W) :-
	(
		query_proof_rao(Key,A,W1) ->
		(
			W is 1-W1
		)
		;
		(
			W=1
		)
	).



/*
query_proof_rao(Key,A ~= Val,W) :-
	(
		recorded(Key,A ~= finite(Distribution),_) ->
		(
			true
		)
		;
		(
			A=next(F),
			user:distributionalclause(bk(F),finite(Distribution),Body,_),
			ground(F),
			ground(Distribution),
			ground(Body),
			Body,
			recorda(Key,next(F) ~= finite(Distribution),_)
		)
	),
	!,
	recorded(Key,A ~= finite(Distribution),_),
	member(W:Val,Distribution).
	
	%findall(W:Val,member(W:Val,Distribution),List).

query_proof_rao(Key,A,1.0) :-
	A\= current(_) ~= finite(_),
	A\= next(_) ~= finite(_),
	query_proof(Key,A).% to check!
	
query_proof_rao(Key,\+A,1.0) :-
	A\= current(_) ~= finite(_),
	A\= next(_) ~= finite(_),
	query_proof(Key,\+A).
*/	


query_proof_setRaoBackward(Key,true,Var,Val,0) :-
	!.
query_proof_setRaoBackward(Key,(A,B),Var,Val,Ignore) :-
	!,
	query_proof_setRaoBackward(Key,A,Var,Val,Ignore1),
	(
		Ignore1==1 ->
			true
		;
			query_proof_setRaoBackward(Key,B,Var,Val,Ignore)
	).

query_proof_setRaoBackward(Key,A,Var,Val,Ignore) :-
	(
		A= next(Var) ~= V ->
		(
			V=Val,
			Ignore=0
		)
		;
		(
			(A= next(Var2) ~= V,user:rao(Var2),Var2\=Var) ->
			(
				Ignore=1
			)
			;
			(
				Ignore=0,
				proof_query_backward(Key,A)
			)
		)
	).


%used in add_rao_backward to add rao variables needed to evaluate the evidence 
query_proof_defineRaoBackward(Key,true) :-
	!.
query_proof_defineRaoBackward(Key,(A,B)) :-
	!,
	query_proof_defineRaoBackward(Key,A),
	query_proof_defineRaoBackward(Key,B).

query_proof_defineRaoBackward(Key,A) :-
	(
	
		(A= next(Var) ~= _,user:rao(Var)) ->
		(
			(\+recorded(Key,next(Var) ~= _,_),\+recorded(Key,current(Var) ~= _,_)) ->
			(
				user:distributionalclause(current(Var),Distribution,Body,_),
				proof_query_backward(Key,Body),
				ground(Var),
				ground(Distribution),
				recorda(Key,current(Var) ~= Distribution,_),
				write(current(Var) ~= Distribution),nl
			)
			;
			true
		)
		;
		(
			proof_query_backward(Key,A)
		)

	).


% verify the query A (2nd argument) with next(Var)=Val
% Ignore used for pruning resolution (body referred to another rao variable)

query_proof_setRao(Key,true,Var,Val,0) :-
	!.
query_proof_setRao(Key,(A,B),Var,Val,Ignore) :-
	!,
	query_proof_setRao(Key,A,Var,Val,Ignore1),
	(
		Ignore1==1 ->
			true
		;
			query_proof_setRao(Key,B,Var,Val,Ignore)
	).

query_proof_setRao(Key,A,Var,Val,Ignore) :-
	(
		A= next(Var) ~= V ->
		(
			V=Val,
			Ignore=0
		)
		;
		(
			(A= next(Var2) ~= V,user:rao(Var2),Var2\=Var) ->
			(
				Ignore=1
			)
			;
			(
				Ignore=0,
				query_proof(Key,A)
			)
		)
	).
	
	
proof_query_backward_lifted(Key,next(Head)) :-
	user:distributionalclause(next(Head),Distribution,Body,_),
 	proof_query_backward_clause(Key,Body,Body2,ListDistributions),
 	%writeln(proof_query_backward_clause(Key,Body,Body2,ListDistributions)),
	%\+ground(Body2),
	computeDistribution(Distribution,ListDistributions,NewDistr),
	test_to_list(NewBody,Body2),
	\+recorded(Key,distributionalclause(next(Head),_,NewBody,_),_),
	recorda(Key,distributionalclause(next(Head),NewDistr,NewBody,0),_).
	%writeln(distributionalclause(next(Head),NewDistr,NewBody,0)).
	
proof_query_backward_lifted2(Key,next(Head)) :-
	
	user:distributionalclause(next(Head),_,_,_),
	\+ground(Head),
	bb_put(l,finite([])),
	findall(W:D,(user:distributionalclause(next(Head),D,Body,_),
 	proof_query_backward_clause2(Key,Body,W),bb_get(l,OL),sum_distrib(OL,D,W,Sum),bb_put(l,Sum)),L),
 	bb_delete(l,LL),LL\=finite([]),%writeln(LL),
	
	\+recorded(Key,distributionalclause(next(Head),_,true,_),_),
	recorda(Key,distributionalclause(next(Head),LL,true,0),_).
%	writeln(distributionalclause(next(Head),LL,true,0)),dcpf:printp(1).
	
% New inference
	
% with a list to store sampled variables. during backtracking sampled random variables may be removed.
proof_query_backward_exp(Key,List,[],true) :-
	!.
	
proof_query_backward_exp(Key,List,NewList,(A,B)) :-
	!,
	proof_query_backward_exp(Key,List,NewList1,A),
	append([List,NewList1],List2),
	proof_query_backward_exp(Key,List2,NewList2,B),
	append([NewList1,NewList2],NewList).


% NOT COMPLETE!
proof_query_backward_exp(Key,Temp,[],findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward_exp(Key,Temp,Temp2,Y),Z),
	!.

proof_query_backward_exp(Key,List,[],\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exp(Key,List,[],A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK problem with negation, if proof_query_backward_exp fails I need to consider the sampled random variables in NewList, that remains ungrounded
%proof_query_backward_exp(Key,List,NewList,\+A) :-
%	\+proof_query_backward_exp(Key,List,NewList,A),!.
% temporary fix!
proof_query_backward_exp(Key,List,[],\+A) :-
	\+proof_query_backward_exp(Key,List,NewList,A),!.

% prototype for negation! not complete
proof_query_backward_exp(Key,List,[not(NewList)],\+A) :-
	proof_query_backward_exp(Key,List,NewList,A),!.


proof_query_backward_exp(Key,List,[],A) :-
	ground(A),
	recorded(Key,A,_),
	A\= _~= distribution(_),
	!.

proof_query_backward_exp(Key,List,[],A) :-
	ground(A),
	memberchk(A,List),
	!.


proof_query_backward_exp(Key,List,[H ~= Val],H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
%	recorda(Key,H ~= Val,_),
	S=Val,
	!.

proof_query_backward_exp(Key,List,[H ~= Val],H~=Val) :-
%	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+member(H~=_,List),
	sample(D,Val).


	
%%% Tabling %%%

proof_query_backward_exp(Key,List,[],A) :-
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_exp(Key,List,[],A) :-
	member(A,List).

proof_query_backward_exp(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	tabling_proof_query_backward_exp2(Key,List,Newvars,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp), %\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exp(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_exp(Key,List,Newvars,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp),%\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exp(Key,List,[Head|Newvars],Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_exp2(Key,List,Newvars,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],Temp),
	\+member(Head,Temp).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).
	
tabling_proof_query_backward_exp2(Key,List,Newvars,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward_exp(Key,List,Newvars,Body).
	
tabling_proof_query_backward_exp2(Key,List,Newvars,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_exp(Key,List,Newvars,Body).


proof_query_backward_exp_eval(Key,List,[],true,1.0) :-
	!.


proof_query_backward_exp_eval(Key,List,NewList,(A,B),W) :-
	!,
	proof_query_backward_exp_eval(Key,List,NewList1,A,W1),
	append([List,NewList1],List2),
	proof_query_backward_exp_eval(Key,List2,NewList2,B,W2),
	append([NewList1,NewList2],NewList),
	W is W1*W2.

% NOT IMPLEMENTED
%proof_query_backward_exp_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
%	findall(X,proof_query_backward(Key,Temp,Y),Z),
%	!.

proof_query_backward_exp_eval(Key,List,[],\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_exp_eval(Key,List,Newvars,\+A,1.0) :-
	\+proof_query_backward_exp(Key,List,Newvars,A),!.

proof_query_backward_exp_eval(Key,List,Newvars,\+A,0.0) :-
	proof_query_backward_exp(Key,List,Newvars,A).

proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.


proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	memberchk(A,List),%recorded(Temp,A,_),
	!.	
	
% TO TEST!!
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward_exp_eval(Key,List,[H ~= Val],H~=Val,W) :-
%	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+member(H~=_,List),
	likelihood_weighting(Val,D,W).


proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	member(A,List).%recorded(Temp,A,_).

proof_query_backward_exp_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward_exp(Key,List,Newvars,Body),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_exp_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_exp(Key,List,Newvars,Body),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_exp_eval(Key,List,[Head|Newvars],Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_exp(Key,List,Newvars,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],List2),
	\+member(Head,List2).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).


%proof_query_backward_exp_eval(Key,List,Newvars,Head,0.0) :-
%	Head\= _ ~= _,
%	user:hardclause(Head,Body,_),
%	\+proof_query_backward_exp(Key,List,Newvars,Body).	

%proof_query_backward_eval(Key,Temp,A,0.0) :-
%	\+proof_query_backward_eval(Key,Temp,A,_).

check_evidence_backward_exp(Key,List,Newvars,Wtot) :-
	findall(H,user:evidence(H,1),L),
	check_evidence_proof_exp(Key,List,Newvars,L,Wtot).

check_evidence_proof_exp(Key,List,[],[],1.0) :- !.

check_evidence_proof_exp(Key,List,Newvars3,[H|ListEvidence],W) :-
	proof_query_backward_exp_eval(Key,List,Newvars,H,W1),
	append([List,Newvars],List2),
	check_evidence_proof_exp(Key,List2,Newvars2,ListEvidence,W2),
	append([Newvars,Newvars2],Newvars3),
	W is W1*W2,
	!.
	
%%%% end new inference %%%

%%% exact %%%
% New inference
	
% with a list to store sampled variables
proof_query_backward_exact(Key,List,[],true) :-
	!.
	
proof_query_backward_exact(Key,List,NewList,(A,B)) :-
	!,
	proof_query_backward_exact(Key,List,NewList1,A),
	append([List,NewList1],List2),
	proof_query_backward_exact(Key,List2,NewList2,B),
	append([NewList1,NewList2],NewList).


% NOT COMPLETE!
proof_query_backward_exact(Key,Temp,[],findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward_exact(Key,Temp,Temp2,Y),Z),
	!.

proof_query_backward_exact(Key,List,[],\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exact(Key,List,[],A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward_exact(Key,List,NewList,\+A) :-
	\+proof_query_backward_exact(Key,List,NewList,A),!.

proof_query_backward_exact(Key,List,[],A) :-
	ground(A),
	recorded(Key,A,_),
	!.

proof_query_backward_exact(Key,List,[],A) :-
	ground(A),
	memberchk(A,List),
	!.

%%% Tabling %%%

proof_query_backward_exact(Key,List,[],A) :-
	recorded(Key,A,_).
	
proof_query_backward_exact(Key,List,[],A) :-
	member(A,List).

proof_query_backward_exact(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	tabling_proof_query_backward_exact2(Key,List,Newvars,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp), %\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exact(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_exact(Key,List,Newvars,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp),%\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exact(Key,List,[Head|Newvars],Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_exact2(Key,List,Newvars,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],Temp),
	\+member(Head,Temp).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).
	
tabling_proof_query_backward_exact2(Key,List,Newvars,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward_exact(Key,List,Newvars,Body).
	
tabling_proof_query_backward_exact2(Key,List,Newvars,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_exact(Key,List,Newvars,Body).


proof_query_backward_exact_eval(Key,List,[],true,1.0) :-
	!.


proof_query_backward_exact_eval(Key,List,NewList,(A,B),W) :-
	!,
	proof_query_backward_exact_eval(Key,List,NewList1,A,W1),
	append([List,NewList1],List2),
	proof_query_backward_exact_eval(Key,List2,NewList2,B,W2),
	append([NewList1,NewList2],NewList),
	W is W1*W2.

% NOT IMPLEMENTED
%proof_query_backward_exact_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
%	findall(X,proof_query_backward(Key,Temp,Y),Z),
%	!.

proof_query_backward_exact_eval(Key,List,[],\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_exact_eval(Key,List,Newvars,\+A,1.0) :-
	\+proof_query_backward_exact(Key,List,Newvars,A),!.

%proof_query_backward_exact_eval(Key,List,Newvars,\+A,0.0) :-
%	proof_query_backward_exact(Key,List,Newvars,A).

proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Key,A,_),
	!.

proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	memberchk(A,List),%recorded(Temp,A,_),
	!.

proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_).
	
proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	member(A,List).%recorded(Temp,A,_).

proof_query_backward_exact_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward_exact_eval(Key,List,Newvars,Body,Wold),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W1)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				exactsampling(Distribution,Var,W1)%, sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				%W=1.0
			)
		)
	),
	W is Wold*W1.
/*
proof_query_backward_exact_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_exact(Key,List,Newvars,Body),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).
*/
proof_query_backward_exact_eval(Key,List,[Head|Newvars],Head,W) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_exact_eval(Key,List,Newvars,Body,W),
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],List2),
	\+member(Head,List2).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).

check_evidence_backward_exact(Key,List,Newvars,Wtot) :-
	findall(H,user:evidence(H,1),L),
	check_evidence_proof_exact(Key,List,Newvars,L,Wtot).

check_evidence_proof_exact(Key,List,[],[],1.0) :- !.

check_evidence_proof_exact(Key,List,Newvars3,[H|ListEvidence],W) :-
	proof_query_backward_exact_eval(Key,List,Newvars,H,W1),
	append([List,Newvars],List2),
	check_evidence_proof_exact(Key,List2,Newvars2,ListEvidence,W2),
	append([Newvars,Newvars2],Newvars3),
	W is W1*W2.
%%% end exact %%%

% New inference
% it works if there is one proof for each variable
eval_query_backward_exp(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward_exp(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		%write('Sampled E '),writeln(Newvars),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward_exp_eval(sampled,Newvars,Newvars2,Query,W2)
			->
			(
				%write('Sampled Q '),writeln(Newvars2),
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		ps,
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

eval_query_backward_exact(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(succeeding_proofs,0),
	(
		%between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		
		check_evidence_backward_exact(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		
		
		%write(W1),write(' Evidence Sampled '),writeln(Newvars),
		
		proof_query_backward_exact_eval(sampled,Newvars,Newvars2,Query,W2),
		bb_get(succeeding_sample_sum,Old),
		New is Old+W1*W2,
		%writeln(New is Old+W1*W2),
		bb_put(succeeding_sample_sum,New),	
		
		bb_get(succeeding_proofs,OldC),
		C is OldC+1,
		bb_put(succeeding_proofs,C),
		
		append([Newvars,Newvars2],Un),
		TempP is New/New_Sum,
		write(TempP),write(' Sampled '),writeln(Un),
		
		(C<N ->
			fail;
			true
		);

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

eval_query_backward_exact_distrib(PosEvidence,NegEvidence,X,Query,N,Distr) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	bb_put(succeeding_proofs,0),
	(
		%between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		
		check_evidence_backward_exact(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		
		
		%write(W1),write(' Evidence Sampled '),writeln(Newvars),
		
		proof_query_backward_exact_eval(sampled,Newvars,Newvars2,Query,W2),
		bb_get(succeeding_sample_sum,OldD),
		
		sum_distrib(finite(OldD),finite([W1:X]),W2,finite(NewD)),
		%write(NewD),nl,
		bb_put(succeeding_sample_sum,NewD),
		
		bb_get(succeeding_proofs,OldC),
		C is OldC+1,
		bb_put(succeeding_proofs,C),
		
		append([Newvars,Newvars2],Un),
		%write(' Sampled '),writeln(Un),
		
		(C<N ->
			fail;
			true
		);

		true
	),

	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		Distr=[]
		;
		divideby(SUCC,TOT,Distr)
	),
	retractall(user:evidence(_,_)),
	!.
	
eval_query_backward_distrib(PosEvidence,NegEvidence,X,Query,N,Distr) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward(sampled,W1),
		check_evidence(sampled,PosEvidence,NegEvidence),
		W1>0,
		%write('Sampled E '),writeln(Newvars),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)
			->
			(
				%write('Sampled Q '),writeln(Newvars2),
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
								
				bb_get(succeeding_sample_sum,Old),
				sum_distrib(finite(Old),finite([W1:X]),1.0,finite(New)),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		%ps,
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		Distr=[]
		;
		divideby(SUCC,TOT,Distr)
	),
	retractall(user:evidence(_,_)),
	!.
	
% NOT COMPLETE
eval_query_backward_exp_distrib(PosEvidence,NegEvidence,X,Query,N,Distr) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward_exp(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		%write('Sampled E '),writeln(Newvars),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward_exp_eval(sampled,Newvars,Newvars2,Query,W2)
			->
			(
				%write('Sampled Q '),writeln(Newvars2),
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
								
				bb_get(succeeding_sample_sum,Old),
				Weight is W1*W2,
				sum_distrib(finite(Old),finite([Weight:X]),1.0,finite(New)),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		%ps,
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		Distr=[]
		;
		divideby(SUCC,TOT,Distr)
	),
	retractall(user:evidence(_,_)),
	!.
	


proof_query_backward_likelihood(Key,true,1.0,[]) :-
	!.
	
proof_query_backward_likelihood(Key,(A,B),W,NewList) :-
	!,
	proof_query_backward_likelihood(Key,A,W1,L1),
	proof_query_backward_likelihood(Key,B,W2,L2),
	append([L1,L2],NewList),
	W is W2*W1.

	
% INCOMPLETE put union of the list!
proof_query_backward_likelihood(Key,findall_forward(X,Y,Z),W,[]) :-
	findall(WI:X,proof_query_backward_likelihood(Key,Y,WI,LI),ZW),
	writeln('INCOMPLETE put union of the list!'),halt,
	product_wlist(ZW,Z,W),
	!.



proof_query_backward_likelihood(Key,\+A,1.0,[]) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_likelihood(Key,A,1.0,[]) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% W not reliable!
proof_query_backward_likelihood(Key,\+A,1,[\+A]) :-
	\+proof_query_backward_likelihood(Key,A,W1,L),
	!.

proof_query_backward_likelihood(Key,A,1.0,[A]) :-
	ground(A),
	recorded(Key,A,_),
	!.

proof_query_backward_likelihood(Key,A,1.0,[A]) :-
	recorded(Key,A,_).

proof_query_backward_likelihood(Key,Head ~= Val,W,[(Head ~= Val,L)]) :-
	tabling_proof_query_backward_likelihood(Key,Head,Distribution,W1,L),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	likelihood_weighting(Var,Distribution,W2),
	recorda(Key,Head ~= Var,_),
	Var=Val,
	W is W1*W2.

proof_query_backward_likelihood(Key,Head,W,[(Head,L)]) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_likelihood(Key,Head,W,L),	
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).


tabling_proof_query_backward_likelihood(Key,Head,Distribution,W,L) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward_likelihood(Key,Body,W,L).

	
tabling_proof_query_backward_likelihood(Key,Head,W,L) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_likelihood(Key,Body,W,L).


proof_query_backward_likelihood(Key,Head ~= Val,W,[(Head ~= Val,L)]) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_likelihood(Key,Body,W1,L),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	likelihood_weighting(Var,Distribution,W2),
	recorda(Key,Head ~= Var,_),
	Var=Val,
	W is W1*W2.


/*

test blockworld:
init_particle([observation(on(6,5))~= true,observation(on(3,2))~= true,observation(on(2,1))~= true,observation(on(1,table))~= true,observation(on(4,table))~= true,observation(clear(3))~= true,observation(on(5,4))~= true,observation(clear(6))~= true],1).
dcpf:step_particle1([action(move(6,table))],[],[],1,1).
distributionalclause:partialproof(1,next(on(6,table)),L).
distributionalclause:partialproof(1,(next(on(2,1)),next(on(6,table))),L).
distributionalclause:proof_query_backward(1,current(reward)~=A),!.
distributionalclause:partialproof(1,(next(on(2,1)),next(on(6,table)),current(reward)~= -1),L),!.
printp(1).
distributionalclause:partialproof(1,\+next(on(6,5)),L).
distributionalclause:partialproof(1,\+ reward ~= _,L).

*/
% partial proof


partialproof(Key,A,ListClean3) :-
	abolish_all_tables,
	partialproof_query_backward(Key,A,List),
%	writeln(List),
	cleanformula(List,ListClean),
	
	partialproof_removetrue(Key,ListClean,ListClean2,True),
%	writeln(ListClean2),
%	writeln(True),
	flatten(True,TrueList),
%	writeln(TrueList),
	remove_duplicates(TrueList, TrueList2),
	test_to_list(TruePruned,TrueList2),
%	writeln(TruePruned),
%	writeln(ListClean2),nl,

	cleanformula2((TruePruned,ListClean2),ListClean3).

partialproof2(Key,A,ListClean3) :-
	abolish_all_tables,

	partialproof_det(Key,A,List),
	
	cleanformula(List,ListClean),
	
	partialproof_removetrue(Key,ListClean,ListClean2,True),
	writeln(l(ListClean2)),
%	writeln(True),
	flatten(True,TrueList),
	writeln(t(TrueList)),
	remove_duplicates(TrueList, TrueList2),
	test_to_list(TruePruned,TrueList2),

	cleanformula2((TruePruned,ListClean2),ListClean3).

regressionproof(Key,Reward,NextState,ListClean3) :-
	abolish_all_tables,
	partialproof_det(Key,Reward,ListReward),
	partialproof_query_backward(Key,NextState,ListNextState),
	cleanformula((ListReward,ListNextState),ListClean),
%	writeln((ListReward,ListNextState)),
	partialproof_removetrue(Key,ListClean,ListClean2,True),
	
	flatten(True,TrueList),
	remove_duplicates(TrueList, TrueList2),
	test_to_list(TruePruned,TrueList2),
%	writeln((TruePruned,ListClean2)),
	cleanformula2((TruePruned,ListClean2),ListClean3).

partialproof3(Key,A,ListClean3) :-
	abolish_all_tables,

	partialproof_det(Key,A,List), % regression over deterministic proof
	
	cleanformula(List,ListClean),
	partialproof_true(Key,ListClean,_,True2),
	flatten(True2,TrueList2),
	remove_duplicates(TrueList2, TrueList3),
	partialproof_removetrue2(Key,ListClean,ListClean2,TrueList3),
%	writeln(l(ListClean2)),
%	writeln(True),
	flatten(TrueList3,TrueList4),
%	writeln(t(TrueList4)),
	remove_duplicates(TrueList4, TrueList5),
	test_to_list(TruePruned,TrueList5),

	cleanformula2((TruePruned,ListClean2),ListClean3).	
	
regressionproof3(Key,Reward,NextState,ListClean3) :-
	abolish_all_tables,
	partialproof_query_backward(Key,NextState,ListNextState),
%	writeln(nextstate(Key,NextState,ListNextState)),
	cleanformula(ListNextState,ListNextState1),
%	writeln(cleanformula(ListNextState,ListNextState1)),
	partialproof_true(Key,ListNextState1,_,TrueNext),
	flatten(TrueNext,TrueNextList),
	remove_duplicates(TrueNextList, TrueNextList2),%trace,
%	writeln(trueNextList(TrueNextList2)),
	partialproof_det(Key,Reward,ListReward,TrueNextList2),
%	writeln(partialproof_det(Key,Reward,ListReward,TrueNextList2)),
	
	cleanformula((ListReward,ListNextState),ListClean),

	partialproof_true(Key,ListClean,_,True2),
	flatten(True2,TrueList2),
	remove_duplicates(TrueList2, TrueList3),
%	writeln(ListClean),nl,
%	writeln(beforetrue2(ListClean)),
	partialproof_removetrue2(Key,ListClean,ListClean2,TrueList3),
%	writeln(partialproof_removetrue2(Key,ListClean,ListClean2,TrueList3)),
	test_to_list(TrueList4,TrueList3),
%	writeln(beforefinal(TrueList4,ListClean2)),
	cleanformula2((TrueList4,ListClean2),ListClean3).
%	writeln(final(ListClean3)),nl.
/*
	flatten(True,TrueList),
	remove_duplicates(TrueList, TrueList2),
	test_to_list(TruePruned,TrueList2),
	cleanformula2((TruePruned,ListClean2),ListClean3).*/
	
cleanformula2(L1,L3) :-
	cleanformula(L1,L2),
	(
	L1==L2 ->
	L3=L2
	;
	cleanformula2(L2,L3)
	).

%flt([],[],[]) :-!.
% flt([],B,B) :-!.
% flt(A,[],A) :-!.

flt(true,B,B) :-!.
flt(A,true,A) :-!.
flt(false,B,false) :-!.
flt(A,false,false) :-!.
flt(A,B,A) :- A==B,!.
flt([A],B,(A,B)) :-!.
flt(A,[B],(A,B)) :-!.
flt(A,B,(A,B)) :-!.

current2next(Key,current(A),next(A)) :- !.
current2next(Key,current(A) ~= V,next(A) ~= V) :- !.
/*
current2next(Key,next(A),true)     :-  recorded(Key,next(A),_),!.
current2next(Key,next(A)~=V,true)  :-  recorded(Key,next(A)~=V,_),!.
current2next(Key,next(A),false)    :- \+recorded(Key,next(A),_),!.
current2next(Key,next(A)~=V,false) :- \+recorded(Key,next(A)~=V,_),!.
*/
current2next(Key,\+A,\+AA) :- current2next(Key,A,AA),!.
current2next(Key,(A,B),(AA,BB)) :-
	!,
	current2next(Key,A,AA),
	current2next(Key,B,BB).

current2next(Key,[A|B],[AA|BB]) :-
	!,
	current2next(Key,A,AA),
	current2next(Key,B,BB).
	
current2next(Key,A,A) :- !.
	
	
	
cleanformula((A,B),NewList) :-
	!,
	cleanformula(A,AA),
	cleanformula(B,BB),
	flt(AA,BB,NewList).

%cleanformula([[],[]],[]) :-
%	!.

cleanformula([A| [false] ],AA) :-
	cleanformula(A,AA),!.
	
cleanformula([false| A ],AA) :-
	cleanformula(A,AA),!.
		


cleanformula([[]|A],AA) :- 
	cleanformula(A,AA),!.

cleanformula(\+ \+ A,AA) :- 
	cleanformula(A,AA),!.
	
cleanformula(\+[],true) :- !.
cleanformula(\+false,true) :- !.
cleanformula(\+true,false) :- !.

cleanformula(\+[false|B],BB) :- 
	cleanformula(\+B,BB),!.

cleanformula(\+[B|[false]],BB) :- 
	cleanformula(\+B,BB),!.
	
cleanformula(\+A,\+ AA) :- 
	cleanformula(A,AA),!.

cleanformula([A| [[]] ],AA) :-
	cleanformula(A,AA),!.
	
cleanformula([A|B],[AA|BB]) :-
	cleanformula(A,AA),
	cleanformula(B,BB),!.
	
cleanformula(A,A) :- !.


partialproof_query_backward(Key,true,true) :-
	!.
	
partialproof_query_backward(Key,(A,B),NewList) :-
	!,
	partialproof_query_backward(Key,A,L1),
	partialproof_query_backward(Key,B,L2),
	flt(L1,L2,NewList).


% INCOMPLETE put union of the list!
partialproof_query_backward(Key,findall_forward(X,Y,Z),true) :-
%	findall(X,partialproof_query_backward(Key,Y,LI),Z),
	writeln('INCOMPLETE put union of the list!'),halt,
	!.



partialproof_query_backward(Key,\+A,true) :-
	user:builtin(A),
	!,
	\+user:A.
		
partialproof_query_backward(Key,A,true) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

partialproof_query_backward(Key,\+A,\+ L) :-
	\+recorded(Key,A,_),
%	trace,writeln(A),
	partialproof_negation(Key,A,AA),
	partialproof_removetrue(Key,AA,L,True1),
	!.

partialproof_query_backward(Key,A ~= V,ProofA) :-
	recorded(Key,A ~= V,_),
	((tabling_partialproof_query_backward(Key,A,D,ProofA),likelihood_weighting(V,D,WVal),WVal>0)->true;ProofA= (A ~= V) ).

partialproof_query_backward(Key,A,ProofA) :-
	recorded(Key,A,_),
	A\=(_ ~= _),
	(tabling_partialproof_query_backward(Key,A,ProofA)->true;ProofA=A).


tabling_partialproof_query_backward(Key,Head,Distribution,L) :-
	user:distributionalclause(Head,Distribution,Body,_),
	query_proof(Key,Body),
 	partialproof_query_backward(Key,Body,L).

	
tabling_partialproof_query_backward(Key,Head,L) :-
	user:hardclause(Head,Body,_),
	query_proof(Key,Body),
	partialproof_query_backward(Key,Body,L).


% proof for reward, regression over deterministic proof
partialproof_det(Key,true,true) :-
	!.
	
partialproof_det(Key,(A,B),NewList) :-
	!,
	partialproof_det(Key,A,L1),
	partialproof_det(Key,B,L2),
	flt(L1,L2,NewList).

partialproof_det(Key,\+A,true) :-
	user:builtin(A),
	!,
	\+user:A.
		
partialproof_det(Key,A,true) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

partialproof_det(Key,\+A,\+L) :-
	\+query_proof(Key,A),
	partialproof_negation(Key,A,AA),
	partialproof_removetrue(Key,AA,L,True1).

partialproof_det(Key,A ~= V,ProofA) :-
	recorded(Key,A ~= V,_),
	((tabling_partialproof_det(Key,A,D,ProofA),likelihood_weighting(V,D,WVal),WVal is 1.0)->true;ProofA= (A ~= V) ).

partialproof_det(Key,A,ProofA) :-
	recorded(Key,A,_),
	A\=(_ ~= _),
	(tabling_partialproof_det(Key,A,ProofA)->true;ProofA=A).


tabling_partialproof_det(Key,Head,Distribution,L) :-
	findall(Body,(user:distributionalclause(Head,Distribution,Body,_),
			query_proof(Key,Body)),ListB),
	sample(uniform(ListB),Body),
%	writeln(sample(uniform(ListB),Body)),trace,
 	partialproof_det(Key,Body,L).

	
tabling_partialproof_det(Key,Head,L) :-
	findall(Body,(user:hardclause(Head,Body,_),
			query_proof(Key,Body)),ListB),
	sample(uniform(ListB),Body),
	partialproof_det(Key,Body,L).

% ------------------

partialproof_det(Key,true,true,TrueList) :-
	!.
	
partialproof_det(Key,(A,B),NewList,TrueList) :-
	!,
	partialproof_det(Key,A,L1,TrueList),
	partialproof_det(Key,B,L2,TrueList),
	flt(L1,L2,NewList).

partialproof_det(Key,A,true,TrueList) :-
	member(A,TrueList).

partialproof_det(Key,\+A,true,TrueList) :-
	user:builtin(A),
	!,
	\+user:A.
		
partialproof_det(Key,A,true,TrueList) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

partialproof_det(Key,\+A,\+L,TrueList) :-
	\+query_proof(Key,A),
	partialproof_negation(Key,A,AA),
	partialproof_removetrue(Key,AA,L,True1).

partialproof_det(Key,A ~= V,ProofA,TrueList) :-
	recorded(Key,A ~= V,_),\+member(A ~= V,TrueList),
	((tabling_partialproof_det2(Key,A,D,ProofA,TrueList),likelihood_weighting(V,D,WVal),WVal is 1.0)->true;ProofA= (A ~= V) ).

partialproof_det(Key,A,ProofA,TrueList) :-
	recorded(Key,A,_),\+member(A,TrueList),
	A\=(_ ~= _),
	(tabling_partialproof_det2(Key,A,ProofA,TrueList)->true;ProofA=A).


tabling_partialproof_det2(Key,Head,Distribution,L,TrueList) :-
	findall(Body,(user:distributionalclause(Head,Distribution,Body,_),
			query_proof(Key,Body)),ListB),
	sample(uniform(ListB),Body),
 	partialproof_det(Key,Body,L,TrueList).

	
tabling_partialproof_det2(Key,Head,L,TrueList) :-
	findall(Body,(user:hardclause(Head,Body,_),
			query_proof(Key,Body)),ListB),
	sample(uniform(ListB),Body),
	partialproof_det(Key,Body,L,TrueList).


% -----

% partialproof_removetrue

partialproof_removetrue(Key,true,true,true) :-
	!.
partialproof_removetrue(Key,[],[],[]) :-
	!.	
partialproof_removetrue(Key,(A,B),NewList,[True1,True2]) :-
	!,
	partialproof_removetrue(Key,A,L1,True1),
	partialproof_removetrue(Key,B,L2,True2),
	flt(L1,L2,NewList).

partialproof_removetrue(Key,[A|B],[L1|L2],[True1|True2]) :-
	!,
	partialproof_removetrue(Key,A,L1,True1),
	partialproof_removetrue(Key,B,L2,True2).
	
partialproof_removetrue(Key,\+A,\+L,True) :-
	partialproof_removetrue(Key,A,L,True),!.

partialproof_removetrue(Key,A,true,B) :-
	ground(A),
	recorded(Key,A,_),
	(A=action(_) -> B=true ; B=A),!.

% not storing random variables with mismatch value.
partialproof_removetrue(Key,A~=V,false,true) :-
	ground(A),
	recorded(Key,A~=V2,_),V\=V2,!.

partialproof_removetrue(Key,A,A,true) :-
	!.%(\+recorded(Key,A,_);\+ground(A)),!.


%% remove atoms in the list True
partialproof_removetrue2(Key,true,true,_) :-
	!.
partialproof_removetrue2(Key,[],[],_) :-
	!.	
partialproof_removetrue2(Key,(A,B),NewList,True) :-
	!,
	partialproof_removetrue2(Key,A,L1,True),
	partialproof_removetrue2(Key,B,L2,True),
	flt(L1,L2,NewList).

partialproof_removetrue2(Key,[A|B],[L1|L2],True) :-
	!,
	partialproof_removetrue2(Key,A,L1,True),
	partialproof_removetrue2(Key,B,L2,True).
	
partialproof_removetrue2(Key,\+A,\+L,True) :-
	partialproof_removetrue2(Key,A,L,True),!.

partialproof_removetrue2(Key,action(A),AA,True) :-
	(
	recorded(Key,action(A),_) ->
	AA=true
	;
	AA=false
	),!.

partialproof_removetrue2(Key,\+action(A),AA,True) :-
	(
	recorded(Key,action(A),_) ->
	AA=false
	;
	AA=true
	),!.


partialproof_removetrue2(Key,A,AA,True) :-
	user:builtin(A),
	(
		ground(A)->
		(
		user:A -> AA=true;AA=false
		)
	;
		AA=A
	),
	!.

partialproof_removetrue2(Key,A,true,True) :-
	member(A,True),!. % check if ground(A) is needed! 

% not storing random variables with mismatch value.
partialproof_removetrue2(Key,A~=V,false,True) :-
	member(A~=V2,True),V\=V2,!.

partialproof_removetrue2(Key,A,A,True) :-
	\+member(A,True),!.

%% end 2

% what is true
% partialproof_true

partialproof_true(Key,true,true,true) :-
	!.
partialproof_true(Key,[],[],[]) :-
	!.	
partialproof_true(Key,(A,B),NewList,[True1,True2]) :-
	!,
	partialproof_true(Key,A,L1,True1),
	partialproof_true(Key,B,L2,True2),
	flt(L1,L2,NewList).

partialproof_true(Key,[A|B],[L1|L2],[True1|True2]) :-
	!,
	partialproof_true(Key,A,L1,True1),
	partialproof_true(Key,B,L2,True2).
	
partialproof_true(Key,\+A,\+A,true) :-
%	partialproof_true(Key,A,L,True),
	!.

partialproof_true(Key,action(A),true,true) :-!.
partialproof_true(Key,\+action(A),true,true) :-!.

partialproof_true(Key,A,AA,B) :-
	(
		ground(A) ->
		(
			recorded(Key,A,_) ->
				(AA=A,B=A)
			;
				(AA=A,B=true)
		)
		;
		(AA=A,B=true)
	),!.
%	copy_term(A,AA),
%	recorded(Key,A,_),
%	(A=action(_) -> (AA=true,B=true) ; (AA=A,B=A)),!.% (ground(AA),B=A)),!.

% not storing random variables with mismatch value.
%partialproof_true(Key,A~=V,false,true) :-
%	recorded(Key,A~=V2,_),V\=V2,!.


partialproof_true(Key,A,AA,true) :-
	user:builtin(A),
	(
		ground(A)->
		(
		user:A -> AA=true;AA=false
		)
	;
		AA=A
	),
	!.

partialproof_true(Key,A,A,true) :-
	\+recorded(Key,A,_),!. %;\+ground(A),!.


% ---- negation

partialproof_negation(Key,false,false) :-
	!.
	
partialproof_negation(Key,(A,B),NewList) :-
	!,
	partialproof_negation(Key,A,L1),
	partialproof_negation(Key,B,L2),
	flt(L1,L2,NewList).



partialproof_negation(Key,\+A,\+ AA) :-
	user:builtin(A),
	(
		ground(A)->
		(
		user:A-> AA=true;AA=false
		)
	;
		AA=A
	),
	!.%,
%	\+user:A.
		
partialproof_negation(Key,A,AA) :-
%	A\=(\+_),
	user:builtin(A),
	(
		ground(A)->
		(
		user:A-> AA=true;AA=false
		)
	;
		AA=A
	),
	!.%,
%	user:A.

partialproof_negation(Key,A,true) :-
	ground(A),
	recorded(Key,A,_),!.

partialproof_negation(Key,\+A,\+ ProofA) :-
%	recorded(Key,A,_),
	(partialproof_negation(Key,A,L) -> ProofA=L ; ProofA=A).%findall(L,partialproof_query_backward(Key,A,L),

/*
partialproof_negation(Key,A,ProofA) :-
	recorded(Key,A,_),
	(partialproof_query_backward(Key,A,L) -> ProofA=L ; ProofA=A). % should be add to the true list

partialproof_negation(Key,A ~= V,ProofA) :-
	recorded(Key,A ~= V2,_),
	V2\==V,
	(partialproof_query_backward(Key,A ~= V2,L) -> ProofA=(A ~= V,L) ; ProofA= A ~= V). % should be add to the true list
*/	
partialproof_negation(Key,A ~= V,ProofA1) :-
%	\+recorded(Key,A ~= _,_),
	copy_term(A ~= V,AA),
	(
	(tabling_partialproof_negation(Key,A,D,ProofA),\+query_proof(Key,ProofA)) ->
	ProofA1=ProofA
	;
	(
%	tabling_partialproof_negation(Key,A,D,ProofA) ->
%		(ProofA1=true,writeln((Key,AA,ProofA)),nl)
%	;
	ProofA1= A ~= V
	)
	).
%	findall(Proof,(tabling_partialproof_negation(Key,A,D,Proof),\+query_proof(Key,Proof)),Lproof),
%	(Lproof==[]-> ProofA= A ~= V ; (Lproof=[SingleP|_] -> ProofA=SingleP ; Lproof = ProofA) ). % (Lproof==[]-> ProofA= A ~= V ; (Lproof=[SingleP] -> ProofA=SingleP ; Lproof = ProofA) ).
	
partialproof_negation(Key,A,ProofA) :-
%	\+recorded(Key,A,_),
	A\=(_ ~= _),
%	(A=action(_) ->
%	ProofA=false; % eliminate other actions
	(
	
	(tabling_partialproof_negation(Key,A,ProofA),\+query_proof(Key,ProofA)) ->
	true
	;
	ProofA = A
		%findall(Proof,(tabling_partialproof_negation(Key,A,Proof),\+query_proof(Key,Proof)),Lproof),
		%(Lproof==[]-> ProofA=A ; (Lproof=[SingleP|_] -> ProofA=SingleP ; Lproof = ProofA) ) % (Lproof==[]-> ProofA=A ; (Lproof=[SingleP] -> ProofA=SingleP ; Lproof = ProofA) )
	). %(tabling_partialproof_negation(Key,A,ProofA)->true;ProofA=[]).


tabling_partialproof_negation(Key,Head,Distribution,L) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	partialproof_negation(Key,Body,L).

	
tabling_partialproof_negation(Key,Head,L) :-
	user:hardclause(Head,Body,_),
	partialproof_negation(Key,Body,L).