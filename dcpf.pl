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

:- module(dcpf,[product_list/2,eval_variance_particle/6,eval_average_particle/5,step_particle_aux/4,step_particle_aux/3,avgvar/3,sum_prob/2,inference/1,set_lifted/1,set_options/1,set_current2nextcopy/1,printkeyp/1,printkeyp2/1,printkey/1,step_particle_aux/5,logIndepOptimalProposals/3,indepOptimalProposals/3,optimalproposal/7,likelihood_weighting/3,normalize/2,printp/1,printp/0,set_inference/1,step_particle/3,step_particle/4,step_particle/5,step_particle_ps/2,step_particle_ps/3,step_particle_ps/4,eval_query_particle/3,eval_query_particle2/4,init_particle/1,init_particle/2,init_particle/3]).

:- use_module('random/sampling.pl').
:- use_module(library(lists)).
%:- current_predicate(user:isprivate/0) -> use_module('distributionalclause.private.pl');
:- use_module(distributionalclause).
:- use_module(library(charsio)).

:- bb_put(initdcpf,false).


:- dynamic user:distributionalclausecopied/4.
:- dynamic user:hardclausecopied/3.

%abolish_all_tables :-!.

printp(Pos) :-
	dcpf:bb_get(offset,Offset),
	I is Offset+Pos,
	bb_get(I,LogTempWeight),
	TempWeight is exp(LogTempWeight),
	writeln(weight(TempWeight)),
	(
		recorded(I,current(A),_),
		writeln(A:t),
		fail;
		true
	),
	(
		recorded(I,current(A) ~=V,_),
		writeln(A:t ~= V),
		fail;
		true
	),
	(
		recorded(I,next(A),_),
		writeln(A:t+1),
		fail;
		true
	),
	(
		recorded(I,next(A)~=V,_),
		writeln(A:t+1 ~= V),
		fail;
		true
	),
	(
		recorded(I,A,_),
		A\=current(_),
		A\=current(_)~=_,
		A\=next(_),
		A\=next(_)~=_,
		writeln(A),
		fail;
		true
	).%,findall(A,(recorded(I,A,_), write(A),nl),_).

getsizep(I,SizeI) :-
	findall(A,(recorded(I,A,_),(A=current(_);A=current(_)~=_)),L),length(L,SizeI).

printkeyp(I) :-
	(
		recorded(I,current(A),_),
		writeln(A:t),
		fail;
		true
	),
	(
		recorded(I,current(A) ~=V,_),
		writeln(A:t ~= V),
		fail;
		true
	),
	(
		recorded(I,next(A),_),
		writeln(A:t+1),
		fail;
		true
	),
	(
		recorded(I,next(A)~=V,_),
		writeln(A:t+1 ~= V),
		fail;
		true
	),
	(
		recorded(I,A,_),
		A\=current(_),
		A\=current(_)~=_,
		A\=next(_),
		A\=next(_)~=_,
		writeln(A),
		fail;
		true
	).

printkeyp2(I) :-
	forall(recorded(I,A,_),writeln(A)).
	
sizep(Pos,Dim) :-
	dcpf:bb_get(offset,Offset),
	I is Offset+Pos,
	findall(A,recorded(I,A,_),L),
	length(L,Dim).
	
averagesizep(Particles,Mean) :-
	bb_put(sumobj,0.0),
	(
		between(1,Particles,Pos),
		sizep(Pos,Dim),
		bb_get(sumobj,OldTOT),
		NewTOT is OldTOT+Dim,
		bb_put(sumobj,NewTOT),
		fail;
		true
	),
	bb_delete(sumobj,T),
	Mean is T/Particles.
	
printkey(V) :-
	findall(A,(recorded(V,A,_), write(A),nl),_).
		
		


%%% particle filter %%%
init_particle(N) :-
	abolish_all_tables,
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	bb_put(offset,0),
	bb_put(loglikelihood,0.0),
	distributionalclause:magic,
	Tot is N*2+2,
	get_max_priority(MaxP),
	eraseall(global),
	(
		between(1,Tot,I),
		eraseall(I),
		bb_put(I,0.0), % log weight
		fail;
		true
	),
	(
		between(1,N,I),
		abolish_all_tables,
		% NEW!
		(
			inference(true) ->
			(
				init_query(I,current(_)),
				init_query(I,current(_) ~= _)
			);
			true
		),
		generate_sample_particlefilter_prior(I,MaxP),
		%bb_put(I,1.0),
		fail;
		true
	),
	retractall(user:hardclause(prior(_),_,_)),
	retractall(user:distributionalclause(prior(_),_,_,_)).

% TO CHECK! init particle with the first observation
init_particle(PosEvidence,N) :-
	init_particle([],PosEvidence,N).
	
init_particle(Actions,PosEvidence,N) :-
	abolish_all_tables,
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	bb_put(offset,0),
	bb_put(loglikelihood,0.0),
	distributionalclause:magic,%_particlefilter,
	Tot is N*2+2,
	get_max_priority(MaxP),
	eraseall(global),
	(
		between(1,Tot,I),
		eraseall(I),
		bb_put(I,0.0),
		fail;
		true
	),
	Offset=0,
	%write('timestep '),write(0),nl,
	%bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		%write('particle '),write(I),nl,
		
		%(TO CHECK!! recently changed)
		(
			inference(true) ->
			(
				init_query(I,current(_)),
				init_query(I,current(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true %init_query_list(I,ListProb)
		),
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions), % assert Actions
		generate_sample_particlefilter_prior(I,MaxP),
		fail;
		true
	),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		recorded(I,observation(_) ~= _,R),
		erase(R),
		fail;
		true
	),
	retractall(user:hardclause(prior(_),_,_)),
	retractall(user:distributionalclause(prior(_),_,_,_)).

get_logw(Pos,W) :-
	bb_get(offset,Offset),
	I is Offset+Pos,
	bb_get(I,W),!.

getwp(Pos,W) :-
	bb_get(offset,Offset),
	I is Offset+Pos,
	bb_get(I,LogW),
	W is exp(LogW),!.

get_particle_distribution(DistrNorm,N) :-
	bb_get(offset,Offset),
	bb_put(distrib,[]),
	bb_put(bestparticle,1),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		bb_get(I,WW),
		(
			WW>0.0 ->
			(
				bb_get(distrib,L),
				D=[(WW:I)|L],
				bb_put(distrib,D),
				
				bb_get(bestparticle,Best),
				Best1 is Offset+Best,
				bb_get(Best1,WBest),
				(
					WW>WBest ->
					bb_put(bestparticle,Pos);
					true
				)
			)
			;
			true
		),
		fail;
		true
	),
	bb_get(bestparticle,Best),
	Best1 is Offset+Best,
	bb_put(bestparticle,Best1),
	bb_get(Best1,WBest),
	%write(WBest),nl,
	bb_delete(distrib,Distribution),
	(
		Distribution=[] ->
		(
			nl,write('ERROR: all particles have weight 0!'),nl,
			% temporal solution: take the 1st particle
			Pos1 is Offset+1,
			DistrNorm=[1.0:Pos1]
		)
		;
		normalize(Distribution,DistrNorm)
	).%write(Distribution),nl,
	

get_logparticle_distribution(DistrNorm,N) :-
	bb_get(offset,Offset),
	bb_put(distrib,[]),
	bb_put(bestparticle,1),
	
	(between(1,N,Pos),
	(
		I is Offset+Pos,
		bb_get(I,WW),
		(
			WW>(-inf) ->
			(
				bb_get(bestparticle,Best),
				Best1 is Offset+Best,
				bb_get(Best1,WBest),
				(
					WW>WBest ->
					bb_put(bestparticle,Pos);
					true
				)
			)
			;
			true
		)
	),fail;true )
	,
	bb_get(bestparticle,Best),
	Best1 is Offset+Best,
	bb_put(bestparticle,Best1),
	bb_get(Best1,WBest),
	(
		WBest==(-inf) ->
		(
			writeln('Error: all particles have weight 0!'),writeln((WBest,Best)),
			(
				between(1,N,Pos1),
				I is Offset+Pos1,
				bb_put(I,0.0),
				fail;
				true
			)
		)
		;
		true
	),
/*	time((
	bb_put(distrib,[]),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		bb_get(I,WW),
		(
			exp(WW-WBest)>0 ->
			(
				bb_get(distrib,L),
				ExpW is exp(WW-WBest),
				D=[(ExpW:I)|L],
				bb_put(distrib,D)
			)
			;
			true
		),
		fail;
		true
	) )),
*/
	(findall(WPos:I,(between(1,N,Pos),I is Offset+Pos,bb_get(I,WW),WPos is exp(WW-WBest),WPos>0),Distribution)),
%	reverse(LDD,LDD2),
%	bb_delete(distrib,Distribution),
	%writeln((LDD2,Distribution)),
%	LDD2==Distribution,
	(
		Distribution=[] ->
		(
			nl,write('ERROR: all particles have weight 0!'),nl,
			% temporal solution: take the 1st particle
			Pos1 is Offset+1,
			DistrNorm=[1.0:Pos1]
		)
		;
		normalize(Distribution,DistrNorm)
	).%write(Distribution),nl,



fast_get_logparticle_distribution(DistrNorm,N) :-
	bb_get(offset,Offset),
%	bb_put(distrib,[]),
%	bb_put(bestparticle,1),
	First is Offset+1,
	bb_get(First,WFirst),
	findall(WPos:I,(between(1,N,Pos),
	(
		I is Offset+Pos,
		bb_get(I,WW),
		WPos is exp(WW-WFirst),WPos>0
	)),Distribution),

	(
		Distribution=[] ->
		(
			nl,write('ERROR: all particles have weight 0!'),nl,
			% temporal solution: take the 1st particle
			Pos1 is Offset+1,
			DistrNorm=[1.0:Pos1]
		)
		;
		normalize(Distribution,DistrNorm)
	).%write(Distribution),nl,

% resampling
findpos(N,[],Cumulative,I) :-!.

findpos(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<1)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					eraseall(I),
					bb_put(I,0.0), % should it be log, i.e. 0, however it is normalized
					(
						recorded(Pos,CC,K),
						%write(CC),nl,
						(
							CC=next(Clause)
							->
								(
									recorda(I,current(Clause),_)%,
									%write((CC,current(Clause))),nl
								)
								;
								(
									CC = next(Clause2) ~= V2
									->
										recorda(I,current(Clause2) ~= V2,_)
									;
									(
										/*(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _ )
										->
											(
												recorda(I,CC,_)

											)
											;*/
											true
									)
								)
						),
						fail;
						true
					),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos(N,T,Cum,I)
				)
			)
		)
		;
		true
	).

findpos2(N,Dist,I1) :-
	bb_put(numx,I1),
	bb_put(posD,1),
	bb_put(cumul,0.0),
	repeat,
	bb_get(x,X),
	bb_get(cumul,Cumulative),
	bb_get(posD,PosD),
	arg(PosD,Dist,WW:Pos),
	Cum is Cumulative+WW,
	%writeln(WW:Pos:Cum),
	(
		X<Cum
		->
		(
			bb_get(numx,I),		
			
			eraseall(I),
			bb_put(I,0.0), % should it be log, i.e. 0, however it is normalized
			(
				recorded(Pos,next(Clause),_),
				recorda(I,current(Clause),_),
							
				fail;
				true
			),
			(
				recorded(Pos,next(Clause2) ~= V2,_),
				recorda(I,current(Clause2) ~= V2,_),
				fail;
				true
			),
			%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
			bb_get(bestparticle,Best),
			(
				Pos==Best ->
				bb_put(bestparticle,I);
				true
			),
			
			NewX is X+1/N,
			bb_put(x,NewX),
			NewI is I+1,
			bb_put(numx,NewI)
			%findpos2(N,[WW:Pos|T],Cumulative,NewI)
		);
		(
			bb_put(cumul,Cum),
			NPosD is PosD+1,
			bb_put(posD,NPosD)
			%findpos2(N,T,Cum,I)
		)
	),
	X>1,!.


% resampling
findpos_aux(N,[],Cumulative,I) :-!.

findpos_aux(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<1)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					eraseall(I),
					bb_put(I,0.0),
					select_inference_particlefilter(Pos,_),% inferencestep_particlefilter_backward3(Pos),
					(
						recorded(Pos,CC,K),
						%write(CC),nl,
						(
							CC=next(Clause)
							->
								(
									recorda(I,current(Clause),_)%,
									%write((CC,current(Clause))),nl
								)
								;
								(
									CC = next(Clause2) ~= V2
									->
										recorda(I,current(Clause2) ~= V2,_)
									;
									(
										(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _ )
										->
											(
												recorda(I,CC,_)

											)
											;
											true
									)
								)
						),
						fail;
						true
					),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					/*bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),*/
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos_aux(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos_aux(N,T,Cum,I)
				)
			)
		)
		;
		true
	).


findpos_aux2(N,[],Cumulative,I) :-!.

findpos_aux2(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<N)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					eraseall(I),
					bb_put(I,0.0),
					(
						recorded(Pos,CC,K),
						recorda(I,CC,_),
						fail;
						true
					),
					select_inference_particlefilter(I,_),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					/*bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),*/
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos_aux2(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos_aux2(N,T,Cum,I)
				)
			)
		)
		;
		true
	).

% Copy particle at position Old to New, where state_t+1 becomes state_t
copyparticles_core(Old,New) :-
	(
		recorded(Old,CC,K),
		%write(CC),nl,
		(
			CC=next(Clause)
			->
			(
				recorda(New,current(Clause),_)%,
				%write((CC,current(Clause))),nl
			)
			;
			(
				CC = next(Clause2) ~= V2
				->
					recorda(New,current(Clause2) ~= V2,_)
				;
				(
					(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
					->
					(
						recorda(New,CC,_)

					)
					;
						true
				)
			)
		),
		fail;
		true
	),!.

% Copy particle at position Old to New, where state_t+1 becomes state_t no other variables
copyparticles_core_clean(Old,New) :-
	forall(recorded(Old,CC,K),
	(
		CC=next(Clause)
		->
		(
			recorda(New,current(Clause),_)%,
			%write((CC,current(Clause))),nl
		)
		;
		(
			CC = next(Clause2) ~= V2
			->
				recorda(New,current(Clause2) ~= V2,_)
			;
			true
		)
	)).


% copy particle Old -> New skipping next values (FIXME! next values are copied!)
plaincopyparticles(Old,New) :-
	%(recorded(Old,next(DDD),_)->(printkeyp2(Old),halt);true),
	(
		recorded(Old,CC,K),
		%write(CC),nl,
		(
			CC=current(Clause)
			->
			(
				recorda(New,current(Clause),_)%,
				%write((CC,current(Clause))),nl
			)
			;
			(
				CC = current(Clause2) ~= V2
				->
					recorda(New,current(Clause2) ~= V2,_)
				;
				(
					(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
					->
					(
						recorda(New,CC,_)

					)
					;
						true
				)
			)
		),
		fail;
		true
	).

% copy particle Old -> New skipping next values, converting current to next
plaincopyparticles2(Old,New) :-
	(
		recorded(Old,CC,K),
		%write(CC),nl,
		(
			CC=current(Clause)
			->
			(
				recorda(New,next(Clause),_)%,
				%write((CC,current(Clause))),nl
			)
			;
			(
				CC = current(Clause2) ~= V2
				->
					recorda(New,next(Clause2) ~= V2,_)
				;
				(
					(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
					->
					(
						recorda(New,CC,_)

					)
					;
						true
				)
			)
		),
		fail;
		true
	).

	
copyparticles(Distribution,Offset,N) :-
	(
		between(1,N,Index),
		I is Offset+Index,
		eraseall(I),
		bb_put(I,(-inf)),
		fail;
		true
	),
	bb_get(bestparticle,Best),
	NewBest is Best-(N-Offset)+Offset,
	bb_put(bestparticle,NewBest),
	
%	length(Distribution,Size),
	(
%		between(1,Size,Index),
		nth1(Index,Distribution,WW:Pos),
		I is Offset+Index,
		(
			eraseall(I),
			LogWW is log(WW),
			bb_put(I,LogWW),
			copyparticles_core(Pos,I)
			/*
			(
				recorded(Pos,CC,K),
				%write(CC),nl,
				(
					CC=next(Clause)
					->
						(
							recorda(I,current(Clause),_)%,
							%write((CC,current(Clause))),nl
						)
						;
						(
							CC = next(Clause2) ~= V2
							->
								recorda(I,current(Clause2) ~= V2,_)
							;
							(
								(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
								->
									(
										recorda(I,CC,_)

									)
									;
									true
							)
						)
				),
				fail;
				true
			)
*/
		),
		fail;
		true
	).

copyparticles_aux2(Offset,N) :-
	(
		between(1,N,Index),
		I is Offset+Index,
		eraseall(I),
		bb_put(I,0.0),
		Pos is (N-Offset)+Index,
		copyparticles_core(Pos,I),
		fail;
		true
	).
	/*,
	bb_get(bestparticle,Best),
	NewBest is Best-(N-Offset)+Offset,
	bb_put(bestparticle,NewBest).*/


copyparticles_aux(Distribution,Offset,N) :-
	(
		between(1,N,Index),
		I is Offset+Index,
		eraseall(I),
		bb_put(I,(-inf)),
		fail;
		true
	),/*
	bb_get(bestparticle,Best),
	NewBest is Best-(N-Offset)+Offset,
	bb_put(bestparticle,NewBest),
	*/
%	length(Distribution,Size),
	(
%		between(1,Size,Index),
		nth1(Index,Distribution,WW:Pos),
		I is Offset+Index,
		(
			eraseall(I),
			LogWW is log(WW),
			bb_put(I,LogWW),
			select_inference_particlefilter(Pos,_),%inferencestep_particlefilter_backward3(Pos),
			
			copyparticles_core(Pos,I)
			/*
			(
				recorded(Pos,CC,K),
				%write(CC),nl,
				(
					CC=next(Clause)
					->
						(
							recorda(I,current(Clause),_)%,
							%write((CC,current(Clause))),nl
						)
						;
						(
							CC = next(Clause2) ~= V2
							->
								recorda(I,current(Clause2) ~= V2,_)
							;
							(
								(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
								->
									(
										recorda(I,CC,_)

									)
									;
									true
							)
						)
				),
				fail;
				true
			)
*/
		),
		fail;
		true
	).

resampling(N) :-
	Rand is random/N,
	bb_get(offset,Offset),
	%get_particle_distribution(Distribution,N),
	(get_logparticle_distribution(Distribution,N)),
%	write(Distribution),nl,
	effectivenumparticles(Distribution,Neff),
	RelN is Neff/N*100,
%	write('effective particles '),write(RelN),nl,
	NewOffset is N-Offset,
	bb_put(offset,NewOffset),
	(
		Neff<N*0.8 ->
		(
			% resampling
%			writeln('resampling'),
			bb_put(x,Rand),
			I is NewOffset+1,
			%Dist=..[d|Distribution],
			(findpos(N,Distribution,0,I)) %time(findpos2(N,Dist,I))
		)
		;
		(copyparticles(Distribution,NewOffset,N)) % no resampling
	).

resampling_aux(N) :-
	Rand is random/N,
	bb_get(offset,Offset),
	(fast_get_logparticle_distribution(Distribution,N)),
%	write(Distribution),nl,
	(effectivenumparticles(Distribution,Neff)),
	RelN is Neff/N*100,
%	write('effective particles '),write(RelN),nl,
	NewOffset is N-Offset,
	
%	write('effective particles '),write(RelN),nl,
	(
		Neff<N*0.8 ->
		(
			% resampling
%			writeln('resampling'),
			bb_put(x,Rand),
			II is NewOffset+1,
			(findpos_aux2(N,Distribution,0,II)),
			(copyparticles_aux2(Offset,N)),
			bb_put(offset,Offset)
		)
		;
		(
		(copyparticles_aux(Distribution,NewOffset,N)), % no resampling
		bb_put(offset,NewOffset)
		)
	),!.
	
resampling_ps(N) :-
	Rand is random/N,
	bb_get(offset,Offset),
	%get_particle_distribution(Distribution,N),
	get_logparticle_distribution(Distribution,N),
	%write(Distribution),nl,
	effectivenumparticles(Distribution,Neff),
	RelN is Neff/N*100,
%	write('effective particles '),write(RelN),nl,
	(
		Neff<N*0.9 ->
		(
%			writeln('resampling'),
			NewOffset is N-Offset,
			bb_put(offset,NewOffset),
			% resampling
			bb_put(x,Rand),
			I is NewOffset+1,
			findpos_ps(N,Distribution,0,I)
		)
		;
		true % no resampling
	).

findpos_ps(N,[],Cumulative,I) :-!.

findpos_ps(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<N)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					eraseall(I),
					bb_put(I,0.0),
					(
						recorded(Pos,CC,K),
						recorda(I,CC,_),
						fail;
						true
					),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos_ps(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos_ps(N,T,Cum,I)
				)
			)
		)
		;
		true
	).

		
eval_weight(I,[],1.0) :- !.

eval_weight(I,[H~=Val|T],P) :-
	eval_weight(I,T,PT),
	user:distributionalclause(next(H),D,Body,_),
	query_proof(I,Body),
	likelihood_weighting(Val,D,PH),
	%write((H,D,Body,Val,PH)),nl,
	P is PT*PH.



assert_list(Key,[]) :- !.
assert_list(Key,[H|T]) :-
	recorda(Key,H,_),
	assert_list(Key,T).

assert_list_next(Key,[]) :- !.
assert_list_next(Key,[H|T]) :-
	recorda(Key,H:t+1,_), % assert at time t+1
	assert_list_next(Key,T).

list_next([],[]) :- !.
list_next([H~=V|T],[next(H)~=V|NT]) :-
	!,
	list_next(T,NT),!.
list_next([H|T],[next(H)|NT]) :-
	H\= _ ~= _,!,
	list_next(T,NT).

step_particle(PosEvidence,N) :-
	step_particle([],PosEvidence,[],N,1.0).

step_particle(Actions,PosEvidence,N) :-
	step_particle1(Actions,PosEvidence,[],N,1.0),
	(resampling(N)).

step_particle(Actions,PosEvidence,N,Delta) :-
	step_particle1(Actions,PosEvidence,[],N,Delta),
	resampling(N).

step_particle(Actions,PosEvidence,Constraints,N,Delta) :-
	step_particle1(Actions,PosEvidence,Constraints,N,Delta),
	resampling(N).

step_particle1(Actions,PosEvidence,Constraints,N,Delta) :-
	%statistics(cputime,[TimeInit,_]),
	retractall(user:deltaT(_)),
	list_next(PosEvidence,PosEvidenceNext),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	bb_put(likelihood,0.0),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	bb_put(sample_sum,0.0),
%	retractall(user:action(_)),
%	forall(member(A,Actions),assert(user:A)),
	%forall
	(between(1,N,Pos),
	(
		I is Offset+Pos,
		
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/2), % FOR TABLING
		%write('particle '),write(I),nl,
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions), % assert Actions
		
		%(TO CHECK!! recently changed)
		
		bb_get(I,LogTempWeight),
		TempWeight is exp(LogTempWeight),
		(
		TempWeight>0.0 ->
		(
		bb_get(initdcpf,InitDCPF),
		/*(
			inference(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true %init_query_list(I,PosEvidence) % add callmagic for weight
		),*/
		%writeln(init(I)),
		(
			inference(backward(_)) ->
			(
				raoblackwellisation(false) ->
				(	% Evaluate weight
					%writeln(beforelogweight_constraints(10,I,PosEvidence,Constraints,P)),
					%logweight_constraints(10,I,PosEvidence,Constraints,P)
					(
					logweight_backward(I,PosEvidence,P) ->
						true;
						( write(logweight_backward(I,PosEvidence,P)),writeln(' failed'),printkeyp(I) )
					)
					%logweight_backward(I,PosEvidence,P)
					%logweight_lw(I,PosEvidenceNext,P)
					%writeln(afterlogweight_constraints(10,I,PosEvidence,Constraints,P))
					%eval_weight_backward(I,PosEvidence,P)%,
					%write('weight '),write(P),nl
				)
				;
					true
			)
			;
			true
		),
		(
			(InitDCPF==false) ->
			select_inference_particlefilter(I,MaxP)
			/*
			(
				inference(backward) ->
					inferencestep_particlefilter_backward3(I)
				;
					(
						inference(backwardlazy) ->
							inferencestep_particlefilter_backward3(I)
						;
							generate_sample_particlefilter(I,MaxP)
					)
			)
			*/
			;
			true
		),
		
		%dcpf:bb_get(offset,Offset),I is Offset+1,findall(A,(recorded(I,A,_),write(A),nl),_),trace,
		(
			raoblackwellisation(true) ->
			(
				% NEW
				add_rao_backward(I,PosEvidence),
				inferencestep_particlefilter_magicoff_rao(I,_),
				select_inference_particlefilter(I,MaxP)
				/*
				%generate_sample_particlefilter(I,MaxP)
				(
					inference(backward) ->
						inferencestep_particlefilter_backward2(I)
					;
						generate_sample_particlefilter(I,MaxP)
				)
				*/
			)
			;
				true
		),
		
		(
			inference(true) ->
				(eval_weight(I,PosEvidence,PP),P is log(PP) )
			;
			(
				raoblackwellisation(true) ->
				(
					nl,nl,write('particle '),write(I),nl,

					%findall(AA,(recorded(I,AA,_),write(AA),nl),_),
					findall(Sum,
						(
							recorded(I,next(VarRao) ~= finite(Distr),R),
							user:rao(VarRao),
							
							write(next(VarRao) ~= finite(Distr)),nl,
							findall(NewP:ValRao,(
										member(Pval:ValRao,Distr),
										
										eval_weightRao(I,PosEvidence,WRao,VarRao,ValRao),
										write(eval_weightR(I,PosEvidence,WRao,VarRao,ValRao)),nl,
										write('Val '),write(ValRao),write(' W '),write(WRao),nl,
										NewP is Pval*WRao
									    ),NewDistr),
							nl,
							findall(AA,(recorded(I,AA,_),write(AA),nl),_),
							nl,
							write('NewDistr '),write(NewDistr),nl,
							sum_prob(NewDistr,Sum),
							divideby(NewDistr,Sum,DistrNorm),
							write('W '),write(Sum),nl,
							write('DistrNorm '),write(DistrNorm),nl,
							erase(R),
							cleanDistribution(DistrNorm,Cleaned,0.0),
							recorda(I,next(VarRao) ~= finite(Cleaned),_)
						),ListSum),
					write('ListSum '),write(ListSum),nl,				
					product_list(ListSum,P1),
					write('------------------'),
%					P=Sum
					% for kalman filter  to complete!!!!!!!
					bb_put(pkalman,1.0),
					(% evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...)
						% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
						user:rao(VarRao),
						user:distributionalclause(next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_),
						%trace,
						proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
						ground(VarRao),
						ground(kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas)),
						
						%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
						recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
						% if not recorded find the prior:
						% TO DO!!
						%
						(
							member(observation(VarRao)~=Vevidence,PosEvidence) ->
							true
							;
							(Vevidence=[])
						),
						write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
						
						kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
						recorda(I,next(VarRao) ~= gaussian(Mpost,CovPost),_),
						
						bb_get(pkalman,PKalman),
						NewPKalman is PKalman*Wkalman,
						bb_put(pkalman,NewPKalman),
						write('Wkalman '),write(Wkalman),nl,
						fail;
						true
					
					),% For new variables: evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...) 
					(
						% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
						user:rao(VarRao),
						user:distributionalclause(next(VarRao),gaussian(M,Cov),Body,_),
						%trace,
						proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
						ground(VarRao),
						ground(gaussian(M,Cov)),
						recorda(I,next(VarRao) ~= gaussian(M,Cov),_),
						%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
						%recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
						% if not recorded find the prior:
						% TO DO!!
						%
						member(observation(VarRao)~=Vevidence,PosEvidence),
						
						%write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
						
						%kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
						
						densityGaussian(M,Cov,Vevidence,Wkalman),
						bb_get(pkalman,PKalman),
						NewPKalman is PKalman*Wkalman,
						bb_put(pkalman,NewPKalman),
						write('Wkalman '),write(Wkalman),nl,
						fail;
						true
					
					),
					bb_delete(pkalman,PtotKalman),
					P is P1*PtotKalman
				)
				;
				(		
					inference(false) ->
					(
						logweight_backward(I,PosEvidence,P)%logweight_constraints(10,I,PosEvidence,Constraints,P) %eval_weight_magicoff(I,PosEvidence,P)
					)
					;
					true %	inference(backward)
					
				)
			)
		),
		
		%write('weight '),write(P),nl,
		bb_get(I,OldWeight),
		NewW is OldWeight+P, % log
		bb_put(I,NewW),
		
		bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		bb_put(likelihood,NewLikelihood)
		)
		;
		true
		)%,getsizep(I,SizeI),writeln(size(SizeI))
		%writeln(('Likelihood ',Likelihood,' NewW ',NewW,' NewLikelihood',NewLikelihood)),
		%bb_put(I,P),
		%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
		
	),fail;true),
	
	bb_get(likelihood,TotLikelihood),
	bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	bb_put(loglikelihood,NewLogLikelihood),
	%writeln(('TotLikelihood',TotLikelihood,' LogLikelihood ',LogLikelihood,'  NewLogLikelihood ',NewLogLikelihood)),
	(
		lifted(true) -> % lifted part
		(
			(distributionalclause:proof_query_backward_lifted2(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	),
	bb_put(initdcpf,false).
	%statistics(cputime,[TimeEnd1,_]),
	%Tempo1 is (TimeEnd1-TimeInit)/1000.0,
	%write('before resampling '),write(Tempo1),nl,
	%printp(1),
	%resampling(N).%,
	%statistics(cputime,[TimeEnd2,_]),
	%Tempo2 is (TimeEnd2-TimeInit)/1000.0,
	%write(Tempo2),nl.

step_particle_aux(Actions,PosEvidence,N) :-
	step_particle_aux(Actions,PosEvidence,[],N,1.0).
step_particle_aux(Actions,PosEvidence,N,Delta) :-
	step_particle_aux(Actions,PosEvidence,[],N,Delta).
step_particle_aux(Actions,PosEvidence,Constraints,N,Delta) :-
	step_particle_aux_main(Actions,PosEvidence,Constraints,N,Delta,true),
	resampling_aux(N).


	
step_particle_aux_main(Actions,PosEvidence,Constraints,N,Delta,Assert) :-
	%statistics(cputime,[TimeInit,_]),
	list_next(PosEvidence,PosEvidenceNext),
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	bb_put(likelihood,0.0),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	bb_put(sample_sum,0.0),
	/*(Assert==true ->
			(
%			assert_list(I,PosEvidence), % assert observations
%			assert_list(I,Actions) % assert Actions
%			retractall(user:action(_)),
%			forall(member(A,Actions),assert(user:A))
			true
			)
			;
			true
	),*/
	
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/2), % FOR TABLING
		%write('particle '),write(I),nl,
		(Assert==true ->
			(
%			assert_list(I,PosEvidence), % assert observations
			assert_list(I,Actions) % assert Actions
			)
			;
			true
		),
		%(TO CHECK!! recently changed)
		
		bb_get(I,LogOldWeight),
		LogOldWeight>(-inf),
%		TempWeight is exp(LogOldWeight),
%		TempWeight>0.0,
		/*
		(
			inference(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true %init_query_list(I,PosEvidence) % add callmagic for weight
		),
		*/
		%(
		%	inference(backward(_)) ->
			(
				%raoblackwellisation(false) ->
				%(	% weight computation
					%logweight_constraints(10,I,PosEvidence,Constraints,P)
					/*(
					logweight_backward(I,PosEvidence,P) ->
						true;
						( write(logweight_backward(I,PosEvidence,P)),writeln(' failed'),printkeyp(I) )
					)*/
					%logweight_backward(I,PosEvidence,P)
					logweight_lw(I,PosEvidenceNext,P)%%eval_weight_backward(I,PosEvidence,P)%,
					%write('weight '),write(P),nl
				%)
				%;
				%	true
			),
		%	;
		%	true
		%),
/*		(
			inference(backward) ->
				inferencestep_particlefilter_backward3(I)
			;
				generate_sample_particlefilter(I,MaxP)
		),*/
		%write('weight '),write(P),nl,
		%bb_get(I,OldWeight),
		NewW is LogOldWeight+P, % log
		bb_put(I,NewW),
		
		bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		bb_put(likelihood,NewLikelihood),
		%writeln(('Likelihood ',Likelihood,' NewW ',NewW,' NewLikelihood',NewLikelihood)),
		%bb_put(I,P),
		%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
		fail;
		true
	),
	bb_get(likelihood,TotLikelihood),
	bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	bb_put(loglikelihood,NewLogLikelihood),
	%writeln(('TotLikelihood',TotLikelihood,' LogLikelihood ',LogLikelihood,'  NewLogLikelihood ',NewLogLikelihood)),
	(
		lifted(true) -> % lifted part
		(
			(distributionalclause:proof_query_backward_lifted2(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	).
/*	,
	Rand is random/N,
%	bb_get(offset,Offset),
	get_logparticle_distribution(Distribution,N),
%	write(Distribution),nl,
	effectivenumparticles(Distribution,Neff),
	RelN is Neff/N*100,
%	write('effective particles '),write(RelN),nl,
	NewOffset is N-Offset,
	bb_put(offset,NewOffset),

	(
		Neff<N/2 ->
		(
			% resampling
			bb_put(x,Rand),
			II is NewOffset+1,
			findpos_aux(N,Distribution,0,II)
		)
		;
		copyparticles_aux(Distribution,NewOffset,N) % no resampling
	).*/

reset_loglikelihood :-
	bb_put(loglikelihood,0.0).

get_loglikelihood(L) :-
	bb_get(loglikelihood,L).

% partitioned sampling
step_particle_ps(PosEvidence,N) :-
	step_particle_ps([],PosEvidence,N,1.0).

step_particle_ps(Actions,PosEvidence,N) :-
	step_particle_ps(Actions,PosEvidence,N,1.0).

step_particle_ps(Actions,PosEvidence,N,Delta) :-
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		%write('particle '),write(I),nl,
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions),
		fail;
		true
	),
	step_particle_ps_cicle(Actions,PosEvidence,N,Delta).

step_particle_ps_cicle(Actions,[],N,Delta) :-
	step_particle(Actions,[],N,Delta),!.

step_particle_ps_cicle(Actions,[H|PosEvid],N,Delta) :-
	(PosEvid=[]) ->
	(
		step_particle(Actions,[H],N,Delta)
	);
	(
		bb_get(offset,Offset),
		get_max_priority(MaxP),
		bb_put(sample_sum,0.0),
		(
			between(1,N,Pos),
			I is Offset+Pos,
			abolish_all_tables,
			
			bb_get(I,LogTempWeight),
			TempWeight is exp(LogTempWeight),
			TempWeight>0.0,
			
			(
				inference(true) ->
				(
					init_query(I,next(_)),
					init_query(I,next(_) ~= _),
					init_query_list(I,[H])
				)
				;
				true %init_query_list(I,PosEvidence) % add callmagic for weight
			),
			
			(
				inference(backward(_)) ->
				(
					raoblackwellisation(false) ->
					(
						eval_weight_backward(I,[H],P)%,
						%write('weight '),write(P),nl
					)
					;
						true
				)
				;
				true
			),/*
		(
			inference(backward) ->
				inferencestep_particlefilter_backward2(I)
			;
				generate_sample_particlefilter(I,MaxP)
		),*/
			%dcpf:bb_get(offset,Offset),I is Offset+1,findall(A,(recorded(I,A,_),write(A),nl),_),trace,
			(
				raoblackwellisation(true) ->
				(
					% NEW
					add_rao_backward(I,[H]),
					inferencestep_particlefilter_magicoff_rao(I,_),
					select_inference_particlefilter(I,MaxP)
					/*
					%generate_sample_particlefilter(I,MaxP)
					(
						inference(backward) ->
							inferencestep_particlefilter_backward2(I)
						;
							generate_sample_particlefilter(I,MaxP)
					)
					*/
				)
				;
					true
			),
			
			(
				inference(true) ->
					eval_weight(I,[H],P)
				;
				(
					raoblackwellisation(true) ->
					(
						nl,nl,write('particle '),write(I),nl,
	
						%findall(AA,(recorded(I,AA,_),write(AA),nl),_),
						findall(Sum,
							(
								recorded(I,next(VarRao) ~= finite(Distr),R),
								user:rao(VarRao),
								
								write(next(VarRao) ~= finite(Distr)),nl,
								findall(NewP:ValRao,(
											member(Pval:ValRao,Distr),
											
											eval_weightRao(I,[H],WRao,VarRao,ValRao),
											write(eval_weightR(I,[H],WRao,VarRao,ValRao)),nl,
											write('Val '),write(ValRao),write(' W '),write(WRao),nl,
											NewP is Pval*WRao
										    ),NewDistr),
								nl,
								findall(AA,(recorded(I,AA,_),write(AA),nl),_),
								nl,
								write('NewDistr '),write(NewDistr),nl,
								sum_prob(NewDistr,Sum),
								divideby(NewDistr,Sum,DistrNorm),
								write('W '),write(Sum),nl,
								write('DistrNorm '),write(DistrNorm),nl,
								erase(R),
								cleanDistribution(DistrNorm,Cleaned,0.0),
								recorda(I,next(VarRao) ~= finite(Cleaned),_)
							),ListSum),
						write('ListSum '),write(ListSum),nl,				
						product_list(ListSum,P1),
						write('------------------'),
%					P=Sum
						% for kalman filter  to complete!!!!!!!
						bb_put(pkalman,1.0),
						(% evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...)
							% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
							user:rao(VarRao),
							user:distributionalclause(next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_),
							%trace,
							proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
							ground(VarRao),
							ground(kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas)),
							
							write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
							recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
							% if not recorded find the prior:
							% TO DO!!
							%
	
							(
								member(observation(VarRao)~=Vevidence,[H]) ->
								true
								;
								(Vevidence=[])
							),
							
							write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
							
							kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
							recorda(I,next(VarRao) ~= gaussian(Mpost,CovPost),_),
							
							bb_get(pkalman,PKalman),
							NewPKalman is PKalman*Wkalman,
							bb_put(pkalman,NewPKalman),
							write('Wkalman '),write(Wkalman),nl,
							fail;
							true
						
						),% For new variables: evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...) 
						(
							% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
							user:rao(VarRao),
							user:distributionalclause(next(VarRao),gaussian(M,Cov),Body,_),
							%trace,
							proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
							ground(VarRao),
							ground(gaussian(M,Cov)),
							recorda(I,next(VarRao) ~= gaussian(M,Cov),_),
							%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
							%recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
							% if not recorded find the prior:
							% TO DO!!
							%
							member(observation(VarRao)~=Vevidence,[H]),
							
							%write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
							
							%kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
							
							densityGaussian(M,Cov,Vevidence,Wkalman),
							bb_get(pkalman,PKalman),
							NewPKalman is PKalman*Wkalman,
							bb_put(pkalman,NewPKalman),
							write('Wkalman '),write(Wkalman),nl,
							fail;
							true
						
						),
						bb_delete(pkalman,PtotKalman),
						P is P1*PtotKalman
					)
					;
					(		
						inference(false) ->
						(
							eval_weight_magicoff(I,[H],P)
						)
						;
						true %	inference(backward)
						
					)
				)
			),
			%write('weight '),write(P),nl,
			bb_get(I,OldWeight),
			NewW is OldWeight+P, %log
			bb_put(I,NewW),%bb_put(I,P),
			%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
			fail;
			true
		),
		resampling_ps(N)
	),
	step_particle_ps_cicle(Actions,PosEvid,N,Delta).

% Auxiliary + partitioned sampling
% partitioned sampling
step_particle_aux_ps(PosEvidence,N) :-
	step_particle_aux_ps([],PosEvidence,[],N,1.0).

step_particle_aux_ps(Actions,PosEvidence,N) :-
	step_particle_aux_ps(Actions,PosEvidence,[],N,1.0).

step_particle_aux_ps(Actions,PosEvidence,Constraints,N,Delta) :-
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	bb_put(likelihood,0.0),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		%write('particle '),write(I),nl,
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions),
		fail;
		true
	),
	step_particle_aux_ps_cicle(Actions,PosEvidence,Constraints,N,Delta).

step_particle_aux_ps_cicle(Actions,[],Constraints,N,Delta) :-
	step_particle(Actions,[],Constraints,N,Delta),!.

step_particle_aux_ps_cicle(Actions,[H|PosEvid],Constraints,N,Delta) :-
	(PosEvid=[]) ->
	(
		step_particle_aux_main(Actions,[H],Constraints,N,Delta,false),
		resampling_aux(N)
	);
	(
		step_particle_aux_main(Actions,[H],Constraints,N,Delta,false),
		resampling_ps(N)
	),
	step_particle_aux_ps_cicle(Actions,PosEvid,Constraints,N,Delta).

% end AUX+part



effectivenumparticles(List,Neff) :-
	bb_put(sumweights,0.0),
	(
		member(WW:I,List),
		(
			WW>0.0 ->
			(
				bb_get(sumweights,Old),
				NewSum is Old+WW*WW,
				bb_put(sumweights,NewSum)
			)
			;
			true
		),
		fail;
		true
	),
	!,
	bb_delete(sumweights,S),
	Neff is 1/S.



eval_weight_magicoff(I,[],1.0) :- !.

eval_weight_magicoff(I,[H~=Val|T],P) :-
	eval_weight_magicoff(I,T,PT),
	%user:hardclause(weight(H,PH),Body,_),
	
	user:distributionalclause(next(H),D,Body,_),
	%write(H~=Val),write(' distr '),write(D),write(' Body '),write(Body),write('  Weight'),write(PH),nl,
	query_proof(I,Body),!,
	likelihood_weighting(Val,D,PH),
	%write(H~=Val),write(' distr '),write(D),write(' Body '),write(Body),write('  Weight'),write(PH),nl,
	P is PT*PH.

test_constraints(I,[],1.0) :- !.

test_constraints(I,[Var~=Val|T],P) :-
	test_constraints(I,T,PT),
	(
		(PT==0.0;PT==0) ->
			P=PT
		;
		(
			user:distributionalclause(next(Var),D,Body,_),
			proof_query_backward(I,Body),!,
			likelihood_weighting(Val,D,PH),!,
			P is PT*PH%,
			%writeln(test_constraints(P))
		)
	),!.

clearnext(I) :-
	(
		(
			recorded(I,next(TT),R),
			%writeln(TT),
			erase(R)
		);
		(
			recorded(I,next(TT) ~= VV,R),
			%writeln(TT~=VV),
			erase(R)
		),
		fail;
		true
	).
	
eval_weight_constraints(Iteration,I,O,C,P) :-
	eval_weight_backward(I,O,W),
	test_constraints(I,C,PC),!,
	%writeln(eval_weight_backward(I,O,W)),
	(
		(PC<1) ->
		(
			%writeln('repeat'),
			(
				Iteration>0 ->
				(
				Next is Iteration-1,
				
				%findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_),\+recorded(I,observation(object(ID))~=_,_) ),Variables),
				findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_) ),Variables),
				
				clearnext(I),
				mcmc(I,Variables,C,8),
				%writeln(eval_weight_constraints(Next,I,O,C,P)),
				eval_weight_constraints(Next,I,O,C,P)
				%writeln(P)
				)
				;
				(
					P=0,
					write('fail')
				)
			)
		)
		;
			P is W*PC
	),!.


logweight_lw(I,O,P) :-
	
	%writeln((OTuple,O)),
	test_to_list(OTuple,O),
	bb_put(distributionalclause:wevidence,1.0),
	%QLW=..[q|O],
	bb_put(distributionalclause:q,O),
	%length(O,NQ),
	%bb_put(distributionalclause:nq,NQ),
	bb_put(dx,0),
	%writeln((OTuple,O)),
	(distributionalclause:proof_query_backward_lw(I,OTuple) ->
		bb_delete(distributionalclause:wevidence,W)
	;
		( write(logweight_backward(I,OTuple,W)),writeln(' failed'),printkeyp(I),W=0)
	),
	%writeln(W),
	P is log(W),!.
logweight_constraints(Iteration,I,O,C,P) :-
	(
	logweight_backward(I,O,W) ->
		true;
		( write(logweight_backward(I,O,W)),writeln(' failed'),printkeyp(I) )
	),
	test_constraints(I,C,PC),!,
	%writeln(eval_weight_backward(I,O,W)),
	(
		(PC<1) ->
		(
			%writeln('repeat'),
			(
				Iteration>0 ->
				(
				Next is Iteration-1,
				
				%findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_),\+recorded(I,observation(object(ID))~=_,_) ),Variables),
				findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_) ),Variables),
				
				clearnext(I),
				mcmc(I,Variables,C,5),
				%writeln(eval_weight_constraints(Next,I,O,C,P)),
				logweight_constraints(Next,I,O,C,P)
				%writeln(P)
				)
				;
				(
					P=(-inf),
					write('fail')
				)
			)
		)
		;
			P is W+log(PC)
	),!.


indepcovariance(1,Var,[Var]) :- !.
indepcovariance(2,Var,[Var,0,0,Var]) :- !.	
indepcovariance(3,Var,[Var,  0,  0,
			   0,Var,  0,
			   0,  0,Var]) :- !.

mcmc_jump(I,[],Constraints,[]) :- !.

mcmc_jump(I,[H~=Val|T],Constraints,[H~=NewTuple|NT]) :-
	mcmc_jump(I,T,Constraints,NT),
%	proof_query_backward(I,H~=Val),
	test_to_list(Val,List),
	length(List,N),
	indepcovariance(N,0.0005,Cov),
	sample(gaussian(List,Cov),NewTuple).
/*
dcpf:mcmc_newsample(1,[next(object(5)),next(object(8))],[],N).
dcpf:bb_get(offset,Offset).

dcpf:mcmc(1,[next(object(5)),next(object(8))],[],W).
*/

erase_variables(I,[]) :- !.

erase_variables(I,[Var~=Val|T]) :-
	erase_variables(I,T),
	(
		(
		recorded(I,Var ~= Val,R),
		erase(R)
		)
		;
		true
	).

add_variables(I,[]) :- !.

add_variables(I,[Var~=Val|T]) :-
	add_variables(I,T),
	recorda(I,Var ~= Val,R).

mcmc_likelihood(I,Variables,Constraints,W) :-
	add_variables(I,Variables),
	test_constraints(I,Constraints,CW),
	erase_variables(I,Variables),
	test_to_list(TupleVariables,Variables),
	proof_query_backward_exp_eval(I,[],NV2,TupleVariables,WV),
	W is CW*WV,!.
/*
test

init_particle(1000).
dcpf:step_particle([action(move(8,(0.0,0.0,0.0)))],[observation(object(5)) ~= (0.2,0.0,0.0),observation(object(8)) ~= (0.4,0.0,0.0)],[],1000,0.400000).
plotdata2(1000).
dcpf:step_particle([action(move(8,(0.04,0.0,0.0)))],[observation(object(5)) ~= (0.2,0.0,0.0)],[],1000,0.400000).
plotdata2(1000).
dcpf:step_particle([action(move(8,(0.03,0.0,0.0)))],[observation(object(5)) ~= (0.22,0.0,0.0)],[],1000,0.400000).
plotdata2(1000).
dcpf:step_particle([action(move(8,(0.08,0.0,0.0)))],[observation(object(5)) ~= (0.3,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle([action(move(8,(0.05,0.0,0.0)))],[observation(object(5)) ~= (0.35,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle([action(move(8,(0.1,0.0,0.0)))],[observation(object(5)) ~= (0.45,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle([action(move(8,(0.1,0.0,0.0)))],[observation(object(5)) ~= (0.55,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle1([action(move(8,(0.1,0.0,0.0)))],[observation(object(5)) ~= (0.65,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:bb_get(offset,Offset).
dcpf:mcmc(1001,[next(object(5))~=A, next(object(8))~=B],[constraint(1)~= true],4).

*/
mcmc(I,Variables,Constraints,Steps) :-
	Steps=<0.0,
	!.

mcmc(I,Variables,Constraints,Steps) :-
%	write(Steps),
%	write(Variables),
	erase_variables(I,Variables),
	mcmc_likelihood(I,Variables,Constraints,OldW),
	mcmc_jump(I,Variables,Constraints,NewValues),
	mcmc_likelihood(I,NewValues,Constraints,NewW),
	(
	(NewW==0.0,OldW==0.0) ->
		Ratio=0.5
		;
		Ratio is NewW/OldW
	),
%	write(NewValues),
%	write(Ratio is NewW/OldW),
	(
	Ratio>=1 ->
		(
			NewStep is Steps-1,
			add_variables(I,NewValues),
			mcmc(I,NewValues,Constraints,NewStep)
		)
		;
		(
			Q is 1-Ratio,
			sample(finite([Ratio:true,Q:false]),Accept),	
			(
			Accept==true ->
			(
				NewStep is Steps-1,
				add_variables(I,NewValues),
				mcmc(I,NewValues,Constraints,NewStep)
			)
			;
			add_variables(I,Variables),
			NewStep2 is Steps-0.1,
			mcmc(I,Variables,Constraints,NewStep2)
			)
		)			
	),!.


	
eval_weight_backward(I,[],1.0) :- !.

eval_weight_backward(I,[Var~=Val|T],P) :-
	eval_weight_backward(I,T,PT),
	user:distributionalclause(next(Var),D,Body,_),
	(
	 (D=kalman2(H,MeanMeas,CovMeas)) ->
	 	(
	 		test_to_list(Val,ListVal),
			eval_weight_backward_kalman_simplified(I,Var~=ListVal,PH)
		)
	;
		(
			%proof_query_backward(I,Body),!,
			(
				inference(backward(classic)) ->
					proof_query_backward(I,Body)
				;
				(
					inference(backward(lazy)),
					proof_query_backward_lazy(I,Body)
				)
			),
			likelihood_weighting(Val,D,PH)
			%writeln((Val,D,PH))
		)
	),!,%writeln((H~=Val,PH)),
	P is PT*PH.

logweight_backward(I,[],0.0) :- !.

logweight_backward(I,[Var~=Val|T],P) :-
	logweight_backward(I,T,PT),!,
	user:distributionalclause(next(Var),D,Body,_),
	(
/*	 (D=kalman2(H,MeanMeas,CovMeas)) ->
	 	(
	 		test_to_list(Val,ListVal),
			%eval_weight_backward_kalman_simplified(I,Var~=ListVal,PH)
			writeln('not implemented')
		)
	;
		(*/
			(
				inference(backward(classic)) ->
					proof_query_backward(I,Body) % proof_query_backward_lw(I,Body,Allevidence)
				;
				(
					inference(backward(lazy)) ->
					proof_query_backward_lazy(I,Body)
					;
					(
					inference(false) ->
						query_proof(I,Body)
					;
						(writeln('noinference'),halt)
					)
				)
			),
			!,
			log_likelihood_weighting(Val,D,PH)
			%writeln((Val,D,PH))
%		)
	),!,%writeln((H~=Val,PH)),
	(
		%D=logfinite(_) ->
		P is PT+PH
		%;
		%P is PT+log(PH)
	).


% for kalman rao simplified NOT COMPLETE!
% the observation observation(Var) is related to the state next(Var) 
eval_weight_backward_kalman_simplified(I,observation(Var)~=Vevidence,P) :-
	%eval_weight_backward(I,T,PT),
	user:distributionalclause(next(observation(Var)),kalman2(H,MeanMeas,CovMeas),Body,_),
	proof_query_backward(I,Body),!,
	
	(
	recorded(I,next(Var) ~= Value,_) ->
		true
		;
		(
			%likelihood_weighting(Val,D,PH),
			user:distributionalclause(next(Var),Distribution,Body2,_),
			proof_query_backward(I,Body2),
			
			ground(Var), 
			ground(Distribution) %,
			%writeln(distributionalclause(next(Var),Distribution,Body2,_))
			%\+recorded(Key,next(Head2) ~= X,_)
		)
	),
	Value=distribution(D), % not implemented when Value is not a distribution 
	(
		%Distribution=gaussian(M,Cov),
		convertgaussian(Distribution,gaussian(M,Cov)),
		kalmanrao_simplified(M,Cov,H,MeanMeas,CovMeas,Vevidence,Mpost,CovPost,P), % there is no prediction step.
		%writeln(kalmanrao_simplified(M,Cov,H,MeanMeas,CovMeas,Vevidence,Mpost,CovPost,P)),
		recorda(I,next(Var) ~= distribution(gaussian(Mpost,CovPost)),_)
		%writeln(next(Var) ~= distribution(gaussian(Mpost,CovPost)))
	).
	%P is PT*PH.
	
% for kalman rao simplified NOT COMPLETE!
convertgaussian(indepGaussians([([M11,M12],[Cov11,Cov12,Cov21,Cov22]),([M21,M22],[Cov211,Cov212,Cov221,Cov222])]),
		gaussian([M11,M12,M21,M22],[Cov11,Cov12,    0,    0,
			  		    Cov21,Cov22,    0,    0,
			  		        0,    0,Cov211,Cov212,
			  		        0,    0,Cov221,Cov222])) :- !.
			  		        
convertgaussian(indepGaussians([([M11,M12],[Cov11,Cov12,Cov21,Cov22]),([M21,M22],[Cov211,Cov212,Cov221,Cov222]),([M31,M32],[Cov311,Cov312,Cov321,Cov322])]),
		gaussian([M11,M12,M21,M22,M31,M32],[Cov11,Cov12,    0,    0,	0,	0,
			  		    	    Cov21,Cov22,    0,    0,	0,	0,
			  		        	0,    0,Cov211,Cov212,	0,	0,
			  		        	0,    0,Cov221,Cov222,	0,	0,
			  		        	0,    0,     0,	    0,Cov311,Cov312,
			  		        	0,    0,     0,	    0,Cov321,Cov322])) :- !.


convertgaussian(indepGaussians([([M1],[Cov1]),([M2],[Cov2]),([M3],[Cov3])]),
		gaussian([M1,M2,M3],[Cov1,   0,   0,
					0,Cov2,   0,
					0,   0,Cov3])) :- !.	
							  		        			  		        
convertgaussian(gaussian(M,C),gaussian(M,C)) :- !.

convertgaussian(uniform(A),uniform(A)) :- !.

/*
convertgaussian(indepGaussians([G1,G2|T]),gaussian(M,C)) :-
	convertgaussian(indepGaussians([G1,G2]),gaussian(Mt,Ct)),
	convertgaussian(indepGaussians([(Mt,Ct)|T]),gaussian(M,C)).
*/
% add rao variables needed to evaluate the evidence 
add_rao_backward(I,[]) :- !.

add_rao_backward(I,[H~=Val|T]) :-
	user:distributionalclause(next(H),D,Body,_),
	query_proof_defineRaoBackward(I,Body),
	add_rao_backward(I,T),!.

% TO CHECK!
add_rao_backward(I,[observation(H)~=Val|T]) :-
	user:distributionalclause(next(H),kalman(_,_,_,_,_,_,_,_),Body,_),
	query_proof_defineRaoBackward(I,Body),
	add_rao_backward(I,T),!.
	
% evaluation weight with rao, p(z_t | a_t, b(i)_t) for a_t (Var) = Val
eval_weightRao(I,[],1.0,Var,Val) :- !.

eval_weightRao(I,[H~=Vevidence|T],P,Var,Val) :-
	eval_weightRao(I,T,PT,Var,Val),
	%user:hardclause(weight(H,PH),Body,_),
	user:distributionalclause(next(H),D,Body,_),
	(
		inference(backward(_)) ->
		(
			query_proof_setRaoBackward(I,Body,Var,Val,Ignore),
			likelihood_weighting(Vevidence,D,PH),
			(
			Ignore==0 ->
				P is PT*PH
			;
				P=PT
			)
		)
		;
		(
			query_proof_setRao(I,Body,Var,Val,Ignore),
			likelihood_weighting(Vevidence,D,PH),
			(
			Ignore==0 ->
				P is PT*PH
			;
				P=PT
			)
		)
	),
	!.

select_inference_particlefilter(I,MaxP) :-
	(
		inference(backward(classic)) ->
			inferencestep_particlefilter_backward2(I)
		;
			(
				inference(backward(lazy)) ->
					inferencestep_particlefilter_backward3(I)
				;
					generate_sample_particlefilter(I,MaxP)
			)
	).

% generate sample handling priority for particle filter
generate_sample_particlefilter(Key,MaxP) :-
	(
		between(0,MaxP,Priority),
		(
			inference(true) -> %forward + magic transformation
				inferencestep_particlefilter(Key,Priority,F)
			;
			(
				inference(false) -> %forward, no magic transformation
				inferencestep_particlefilter_magicoff(Key,Priority,F)
				;
				(
					%inference(backward) ->
					%	inferencestep_particlefilter_backward(Key,Priority,F)
					%;
						( write('Error: Inference method not selected!'),nl,fail )
				)
			)
		),
		fail;
		true
	).
	
generate_sample_particlefilter_prior(Key,MaxP) :-
	(
		inference(true) ->
		forall(between(0,MaxP,Priority),inferencestep_particlefilter(Key,Priority,F)) % to test
		;
		(
			inference(backward(_)) ->
			(
				inferencestep_particlefilter_backward_prior(Key,_,F),
				(
					recorded(Key,prior(A),R),
					recorda(Key,current(A),_),
					erase(R),
					fail;
					true
				),
				(
					recorded(Key,prior(A) ~= Val,R),
					recorda(Key,current(A) ~= Val,_),
					erase(R),
					fail;
					true
				)
			)
			;
				forall(between(0,MaxP,Priority),inferencestep_particlefilter_magicoff_prior(Key,Priority,F)) % to test
		)
	).


% for inference(true)
inferencestep_particlefilter(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		\+derived(Key,Head),
		ground(Head),
		%\+recorded(Key,Head,_),
		/*
		(
			Head\=callmagic(_) ->
				ground(Head)
			;
				true
		),
		*/
		recorda(Key,Head,_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

	
% for inference(false)
inferencestep_particlefilter_magicoff(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,Head,_),
		recorda(Key,Head,_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		Head2\=next(observation(_)),
		%Head2\=bk(_),
		
		% RAO
		(
			raoblackwellisation(true) ->
			(
				
				(Head2=current(Vr);Head2=next(Vr))  ->
				(
					\+user:rao(Vr)
				)
				;
				true
				
			)
			;
			true
		),
		
		%(Head2=next(objectProposal(ID)) -> (trace,debug);true),
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_magicoff(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

% for inference(false) for current (eval_query_particle)
inferencestep_particlefilter_magicoff_current(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		Head=current(_),
		query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,Head,_),
		recorda(Key,Head,_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		Head2=current(_),
		Head2\=next(observation(_)),
		%Head2\=bk(_),
		
		% RAO
		(
			raoblackwellisation(true) ->
			(
				
				(Head2=current(Vr);Head2=next(Vr))  ->
				(
					\+user:rao(Vr)
				)
				;
				true
				
			)
			;
			true
		),
		
		%(Head2=next(objectProposal(ID)) -> (trace,debug);true),
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_magicoff(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

% for inference(backward)
inferencestep_particlefilter_backward(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		%Head\=_:0,
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,Head,_),
		recorda(Key,Head,_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		Head2\=next(observation(_)),
		%Head2\=_:0,
		% RAO
		(
			raoblackwellisation(true) ->
			(
				
				(Head2=current(Vr);Head2=next(Vr)) ->
				(
					\+user:rao(Vr)
				)
				;
				true
				
			)
			;
			true
		),
		%
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_backward(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

	


% prediction step (step (2) in the paper)
inferencestep_particlefilter_backward2(Key) :-
	bb_put(flag,false),
	(
		user:hardclause(next(Head),Body,_),
		\+user:hardclausecopied(next(Head),Body,_),%distributionalclause:containscurrent(Body),
		proof_query_backward(Key,Body),
		ground(Head),
		\+recorded(Key,next(Head),_),
		recorda(Key,next(Head),_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		user:distributionalclause(next(Head2),Distribution,Body,_),
		Head2\=observation(_),
		\+user:distributionalclausecopied(next(Head2),Distribution,Body,_),%distributionalclause:containscurrent(Body),
		% RAO
		(
			raoblackwellisation(true) ->
			(
				\+user:rao(Head2)				
			)
			;
			true
		),
		%
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,next(Head2) ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,next(Head2) ~= Val,_),
		bb_put(flag,true),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
			inferencestep_particlefilter_backward2(Key)
		;
			true
	).




% for inference(backward) init prior
inferencestep_particlefilter_backward_prior(Key,Pr,Flag) :-
	(
		lifted(true) -> % Lifted part
		(
			user:distributionalclause(prior(Head2),Distribution,Body,Pr),
			(
					\+(( ground(Head2), ground(Distribution), ground(Body)) ),
					\+recorded(global,distributionalclause(current(Head2),_,Body,_),_),
					recorda(global,distributionalclause(current(Head2),Distribution,(ground(Head2),Body),Pr),_),
					retract(user:distributionalclause(prior(Head2),Distribution,Body,Pr))
			),
			fail;
			true
		);
		true
	),
	inferencestep_particlefilter_backward_prior_main(Key,Pr,Flag).

inferencestep_particlefilter_backward_prior_main(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(prior(Head),Body,Pr),
		proof_query_backward(Key,Body),
		ground(Head),
		\+recorded(Key,prior(Head),_),
		recorda(Key,prior(Head),_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(prior(Head2),Distribution,Body,Pr),
		% RAO
		(
			raoblackwellisation(true) ->
			(
				
				user:rao(Head2) ->
				(
					query_proof_rao(Key,Body,_),
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,prior(Head2) ~= _,_),
					recorda(Key,prior(Head2) ~= Distribution,_)
				)
				;
				(
					proof_query_backward(Key,Body),
		
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,prior(Head2) ~= X,_),
		
					sample(Distribution,Val),
				
					recorda(Key,prior(Head2) ~= Val,_)
				)
				
			)
			;
			(
				proof_query_backward(Key,Body),
	
				ground(Head2), 
				ground(Distribution),
				\+recorded(Key,prior(Head2) ~= X,_),
	
				sample(Distribution,Val),
			
				recorda(Key,prior(Head2) ~= Val,_)
			)
		),
		%
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_backward_prior_main(Key,Pr,_)
		);
		(
			Flag=false
		)
	).
	

	
% for inference(false) init prior
inferencestep_particlefilter_magicoff_prior(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(prior(Head),Body,Pr),
		query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,current(Head),_),
		recorda(Key,current(Head),_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(prior(Head2),Distribution,Body,Pr),
		% RAO
		(
			raoblackwellisation(true) ->
			(
				
				user:rao(Head2) ->
				(
					query_proof_rao(Key,Body,_),
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,current(Head2) ~= _,_),
					recorda(Key,current(Head2) ~= Distribution,_)
				)
				;
				(
					query_proof(Key,Body),
		
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,current(Head2) ~= X,_),
		
					sample(Distribution,Val),
				
					recorda(Key,current(Head2) ~= Val,_)
				)
				
			)
			;
			(
				query_proof(Key,Body),
	
				ground(Head2), 
				ground(Distribution),
				\+recorded(Key,current(Head2) ~= X,_),
	
				sample(Distribution,Val),
			
				recorda(Key,current(Head2) ~= Val,_)
			)
		),
		%
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_magicoff_prior(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

% 

% bayesfilter TO CHECK!

init_bayesfilter :-
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	distributionalclause:magic,
	eraseall(bayes),
	eraseall(bayes2),
	init_query(bayes,current(_,_,_)).
	
bayesfilter_weight([],1.0) :- !.

bayesfilter_weight([H|T],P) :-
	bayesfilter_weight(T,PT),
	query_proof(I,weight(H,PH)),
	P is PT*PH.
	
list_weight([],[]) :-!.
list_weight([H|T],[weight(H,_,_,_)|T2]) :-
	list_weight(T,T2).

getaveragesize(N,Avg) :-
	bb_put(sum,0.0),
	bb_get(offset,Offset),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		getsizep(I,S),
		bb_get(sum,Old),
		New is Old+S,
		bb_put(sum,New),
		fail;
		true
	),bb_delete(sum,Sum),
	Avg is Sum/N.
	
eval_query_particle(Q,N,P) :-
	timesyntax(Q,Query),
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/3), % FOR TABLING
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				inference(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				(
				inference(false) ->
					(
					between(0,MaxP,Priority),
					
					inferencestep_particlefilter_magicoff_current(I,Priority,_),
					fail;true
					)
					;
					true
				)
			),
			%
			(
				raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					inference(backward(Binference)) ->
					(
						
						eraseall(tempparticle),
						%W=1, % error in proof_query_backward_eval
						%proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
						(
						Binference==classic ->
						(
							(test_to_list(Query,Qlist),bb_put(distributionalclause:wevidence,1.0), bb_put(distributionalclause:q,Qlist),distributionalclause:proof_query_backward_lw(I,tempparticle,Query,_),bb_delete(distributionalclause:wevidence,W))
							%proof_query_backward_eval(I,tempparticle,Query,W)
							->
							(
								%write(Query),nl,
								bb_get(succeeding_sample_sum,Old),
								New is Old+Weight*W,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  	);
					  	(
					  		Binference==lazy,
					  		(
					  		proof_query_backward_lazy_eval(I,tempparticle,Query,W)
							->
							(
								%write(Query),nl,
								bb_get(succeeding_sample_sum,Old),
								New is Old+Weight*W,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
						  	)
					  	)
					  	)
					)
					;
					(
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	P is SUCC/TOT,
	!.

eval_query_particle_alternative(Q,N,P) :-
	timesyntax(Q,Query),
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/3), % FOR TABLING
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				inference(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					inference(backward(Binference)) ->
					(
						eraseall(tempparticle),
						%W=1, % error in proof_query_backward_eval
						proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
						%proof_query_backward_eval(I,tempparticle,Query,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					(
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	P is SUCC/TOT,
	!.

eval_query_particle_alternative2(Q,N,P) :-
	timesyntax(Q,Query),
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/3), % FOR TABLING
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				inference(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					inference(backward(Binference)) ->
					(
						eraseall(tempparticle),
						W=1, % error in proof_query_backward_eval
						(
						Binference==classic ->
						(
							proof_query_backward(I,tempparticle,Query) %proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
							%proof_query_backward_eval(I,tempparticle,Query,W)
							->
							(
								%write(Query),nl,
								bb_get(succeeding_sample_sum,Old),
								New is Old+Weight*W,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  	);
					  	(
					  		Binference==lazy,
					  		(
					  		proof_query_backward_lazy(I,tempparticle,Query) %proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
							%proof_query_backward_eval(I,tempparticle,Query,W)
							->
							(
								%write(Query),nl,
								bb_get(succeeding_sample_sum,Old),
								New is Old+Weight*W,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
						  	)
					  	)
					  	
					  	)
					)
					;
					(
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	P is SUCC/TOT,
	!.
	
% non-ground query like right(g,X) IMPLEMENTED ONLY FOR BACKWARD!
% output distribution of each X
eval_query_particle2(X,Q,N,P) :-
	%timesyntax(Q,Query),
	Q=Query,
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,[]),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				inference(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				(
				inference(false) ->
					(
					between(0,MaxP,Priority),
					
					inferencestep_particlefilter_magicoff_current(I,Priority,_),
					fail;true
					)
					;
					true
				)
			),
			%
			(
				% NOT IMPLEMENTED. old code
				raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					inference(backward(Binference)) ->
					(
						eraseall(tempparticle),
						%findall(W:X,(proof_query_backward_exp_eval(I,[],NV2,Query,W)),List)
						(
						Binference==classic ->
						(
							findall(1:X,(distributionalclause:proof_query_backward(I,tempparticle,Query)),List)%findall(1:X,(distributionalclause:proof_query_backward_exp(I,[],NV2,Query)),List)
							->
							(
								
								bb_get(succeeding_sample_sum,Old),
								sum_distrib(finite(Old),finite(List),Weight,finite(New)),
								%write((List,Weight,New)),nl,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  	)
					  	;
					  	(
					  		Binference==lazy,
					  		(
					  		findall(1:X,(distributionalclause:proof_query_backward_lazy(I,tempparticle,Query)),List)%findall(1:X,(distributionalclause:proof_query_backward_exp(I,[],NV2,Query)),List)
							->
							(
								
								bb_get(succeeding_sample_sum,Old),
								sum_distrib(finite(Old),finite(List),Weight,finite(New)),
								%write((List,Weight,New)),nl,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  		)
					  	)
					  	
					  	)
					)
					;
					( % NOT IMPLEMENTED. old code
						findall(1:X,query_proof(I,Query),List)
						->
						(
							%writeln(findall(1:X,query_proof(I,Query),List)),
							bb_get(succeeding_sample_sum,Old),
							sum_distrib(finite(Old),finite(List),Weight,finite(New)),
							%write((List,Weight,New)),nl,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		P=[]
		;
		divideby(SUCC,TOT,P)
	),
	%P is SUCC/TOT,
	!.
	
eval_variance_particle(Avg,Var,X,Query,N,P) :-
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,[]),
	bb_put(sample_count,[]),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				inference(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				% NOT IMPLEMENTED. old code
				raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					inference(backward(Binference)) ->
					(
						eraseall(tempparticle),
						(
						Binference==classic ->
						(
							findall(WVal:X,(member(AvgX:X,Avg),proof_query_backward(I,tempparticle,Query~=Val),diffsquared_multidim(Weight,Val,AvgX,WVal)),List)
							->
							(
								findall(Weight:X,proof_query_backward(I,tempparticle,Query~=Val),Counts),
								bb_get(sample_count,OldCounts),
								sum_list_multidim(OldCounts,Counts,NewCounts),
								bb_put(sample_count,NewCounts),
								
								bb_get(succeeding_sample_sum,Old),
								sum_list_multidim(Old,List,New),
								%write((OldCounts,Counts,NewCounts,Old,List,New)),nl,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  	);
					  	(
					  		Binference==lazy,
					  		(
					  		findall(WVal:X,(member(AvgX:X,Avg),proof_query_backward_lazy(I,tempparticle,Query~=Val),diffsquared_multidim(Weight,Val,AvgX,WVal)),List)
					  		%findall(WVal:X,(proof_query_backward_lazy(I,tempparticle,Query~=Val),prod_scalar_multidim(Val,Weight,WVal)),List)
							->
							(
								findall(Weight:X,proof_query_backward(I,tempparticle,Query~=Val),Counts),
								bb_get(sample_count,OldCounts),
								sum_list_multidim(OldCounts,Counts,NewCounts),
								bb_put(sample_count,NewCounts),
								
								bb_get(succeeding_sample_sum,Old),
								sum_list_multidim(Old,List,New),
								%write((OldCounts,Counts,NewCounts,Old,List,New)),nl,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  		)
					  	
					  	)
					  	)
					)
					;
					( % NOT IMPLEMENTED. old code
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	bb_delete(sample_count,CList),
	
	(
		SUCC==[] ->
		P=[]
		;
		divideby(CList,TOT,P)
	),
	divide_multidim(SUCC,CList,Var),
%	write((SUCC,TOT,CList,Avg)),nl,
	%P is SUCC/TOT,
	!.

eval_average_particle(Avg,X,Query,N,P) :-
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,[]),
	bb_put(sample_count,[]),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				inference(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				% NOT IMPLEMENTED. old code
				raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					inference(backward(Binference)) ->
					(
						eraseall(tempparticle),
						(
						Binference==classic ->
						(
							findall(WVal:X,(proof_query_backward(I,tempparticle,Query~=Val),prod_scalar_multidim(Val,Weight,WVal)),List)
							->
							(
								findall(Weight:X,proof_query_backward(I,tempparticle,Query~=Val),Counts),
								bb_get(sample_count,OldCounts),
								sum_list_multidim(OldCounts,Counts,NewCounts),
								bb_put(sample_count,NewCounts),
								
								bb_get(succeeding_sample_sum,Old),
								sum_list_multidim(Old,List,New),
								%write((OldCounts,Counts,NewCounts,Old,List,New)),nl,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  	);
					  	(
					  		Binference==lazy,
					  		(
					  		findall(WVal:X,(proof_query_backward_lazy(I,tempparticle,Query~=Val),prod_scalar_multidim(Val,Weight,WVal)),List)
							->
							(
								findall(Weight:X,proof_query_backward(I,tempparticle,Query~=Val),Counts),
								bb_get(sample_count,OldCounts),
								sum_list_multidim(OldCounts,Counts,NewCounts),
								bb_put(sample_count,NewCounts),
								
								bb_get(succeeding_sample_sum,Old),
								sum_list_multidim(Old,List,New),
								%write((OldCounts,Counts,NewCounts,Old,List,New)),nl,
								bb_put(succeeding_sample_sum,New)
							)
							;
						  	true
					  		)
					  	
					  	)
					  	)
					)
					;
					( % NOT IMPLEMENTED. old code
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	bb_delete(sample_count,CList),
	
	(
		SUCC==[] ->
		P=[]
		;
		divideby(CList,TOT,P)
	),
	divide_multidim(SUCC,CList,Avg),
%	write((SUCC,TOT,CList,Avg)),nl,
	%P is SUCC/TOT,
	!.

	
%%%% private part
inferencestep_particlefilter_backward3(Key) :-
	bb_put(flag,false),
	(
		user:hardclause(next(Head),Body,_),
		distributionalclause:containscurrent(Body),
		proof_query_backward_lazy(Key,Body),
		(
			ground(Head) ->
			true;
			writeln(notground(Head))
		),
		\+recorded(Key,next(Head),_),
		recorda(Key,next(Head),_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		user:distributionalclause(next(Head2),Distribution,Body,_),
		Head2\=observation(_),
		distributionalclause:containscurrent(Body),
		% RAO
		/*
		(
			raoblackwellisation(true) ->
			(
				\+user:rao(Head2)				
			)
			;
			true
		),
		*/
		%
		proof_query_backward_lazy(Key,Body),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,next(Head2) ~= X,_),
		
		%sample(Distribution,Val),
		
		recorda(Key,next(Head2) ~= distribution(Distribution),_),
		bb_put(flag,true),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
			inferencestep_particlefilter_backward3(Key)
		;
			true
	).
	

/*
inferencestep_particlefilter_backward2_likelihood(Key) :-
	bb_put(flag,false),
	(
		user:hardclause(next(Head),Body,_),
		proof_query_backward_likelihood(Key,Body,W1),
		ground(Head),
		\+recorded(Key,next(Head),_),
		recorda(Key,next(Head),_),
		bb_put(flag,true),
		bb_get(backward2_likelihood,Wold),
		WNew is Wold*W1,
		bb_put(backward2_likelihood,WNew),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		user:distributionalclause(next(Head2),Distribution,Body,_),
		Head2\=observation(_),
		% RAO
		(
			raoblackwellisation(true) ->
			(
				\+user:rao(Head2)				
			)
			;
			true
		),
		%
		proof_query_backward_likelihood(Key,Body,W2),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,next(Head2) ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,next(Head2) ~= Val,_),
		bb_put(flag,true),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		likelihood_weighting(Val,Distribution,W3),
		bb_get(backward2_likelihood,Wold2),
		WNew2 is Wold2*W2*W3,
		bb_put(backward2_likelihood,WNew2),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
			inferencestep_particlefilter_backward2(Key)
		;
			true
	).
*/
% for rao-blackwellised magic=off,stratification not used for rao vars.
inferencestep_particlefilter_magicoff_rao(Key,_) :-
	/*
	(
		user:distributionalclause(current(VarRao),Distribution,Body,Priority),
		user:rao(VarRao),
		query_proof_rao(Key,Body,Weight),
		ground(VarRao), 
		ground(Distribution),
		\+recorded(Key,current(VarRao) ~= _,_),
		recorda(Key,current(VarRao) ~= Distribution,_),
		fail;
	
		true
	),
	*/
	
	% evaluation of clauses current() for rao variables NOT REALLY USED, TO CHECK!
	/*
	(
		
		user:distributionalclause(current(VarRao),Distribution,Body,_),
		user:rao(VarRao),
		
		proof_query_backward(Key,Body),
		

		ground(VarRao),
		ground(Distribution),
		%trace,
		%write(VarRao),nl,write(' Distribution '),write(Distribution),nl,
		\+recorded(Key,current(VarRao) ~= _,_),
		%write('new'),nl,
		recorda(Key,current(VarRao) ~= Distribution,_), % TO CHECK! is not current??
		%write(next(VarRao) ~= Distribution),nl,
		fail;
	
		true
		
	),*/
	%nl,write(Key),nl,
	%user:rao(VarRao1),
	%recorded(Key,current(VarRao1) ~= CurrentDistr,_),
	%write(current(VarRao1) ~= CurrentDistr),nl,
	(% evaluation of clauses next() for rao variables with a discrete distribution: finite(...)
		
		
		user:distributionalclause(next(VarRao),finite(Distribution),Body,_),
		user:rao(VarRao),
		%trace,
		query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
		ground(VarRao),
		ground(Distribution),
		
		%write((Body,Weight)),nl,
		%write(VarRao),nl,write(' Distribution '),write(finite(Distribution)),nl,
		(
			recorded(Key,next(VarRao) ~= OldDistr,R) ->
			(
				sum_distrib(OldDistr,finite(Distribution),Weight,finite(NewDist)), % NewDist = OldDistr + Distribution * Weight
				%write(next(VarRao) ~= finite(NewDist)),nl,
				erase(R),
				cleanDistribution(NewDist,CleanedDistr,0.0),
				%write(next(VarRao) ~= finite(CleanedDistr)),nl,
				recorda(Key,next(VarRao) ~= finite(CleanedDistr),_)
			)
			;
			(
				%write('new'),nl,
				finite(Temp)=finite(Distribution),
				multiplyby(Temp,Weight,NewD),
				recorda(Key,next(VarRao) ~= finite(NewD),_)
				%write(next(VarRao) ~= finite(NewD)),nl
			)
		),
		
		fail;
	
		true
	).
	% add weight evaluation to add clauses from bk!.
	
	
inferencestep_particlefilter_backward_lazy_prior(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(prior(Head),Body,Pr),
		proof_query_backward_lazy(Key,Body),
		ground(Head),
		\+recorded(Key,prior(Head),_),
		recorda(Key,prior(Head),_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(prior(Head2),Distribution,Body,Pr),
		% RAO
		(
			raoblackwellisation(true) ->
			(
				
				user:rao(Head2) ->
				(
					query_proof_rao(Key,Body,_),
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,prior(Head2) ~= _,_),
					recorda(Key,prior(Head2) ~= Distribution,_)
				)
				;
				(
					proof_query_backward_lazy(Key,Body),
		
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,prior(Head2) ~= X,_),
		
					sample(Distribution,Val),
				
					recorda(Key,prior(Head2) ~= Val,_)
				)
				
			)
			;
			(
				proof_query_backward_lazy(Key,Body),
	
				ground(Head2), 
				ground(Distribution),
				\+recorded(Key,prior(Head2) ~= X,_),
	
				sample(Distribution,Val),
			
				recorda(Key,prior(Head2) ~= Val,_)
			)
		),
		%
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_backward_lazy_prior(Key,Pr,_)
		);
		(
			Flag=false
		)
	),
	(
		lifted(true) -> % Lifted part
		(
			user:distributionalclause(prior(Head2),Distribution,Body,Pr),
			(
					\+(( ground(Head2), ground(Distribution), ground(Body)) ),
					\+recorded(global,distributionalclause(current(Head2),_,Body,_),_),
					recorda(global,distributionalclause(current(Head2),Distribution,Body,0),_)
			),
			fail;
			true
		);
		true
	).