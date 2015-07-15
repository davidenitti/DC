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

%:- style_check(all).
:- yap_flag(unknown,error).

:- module(sampling,[samplefinite/2,checkgoallw/3,betaPdf/4,avgvar/3,listmax/2,variance_wis/5,variance/3,initmap/1,addvaluemap/3,deletemap/1,averagemap/2,weightedaverage/3,finiteweight/3,uniformweight/3,normalgsl/3,setseed/1,sumvector/3,logdirichlet_score/3,logbetafunction/2,dirichlet_score/5,poissonPdf/3,betafunction/3,sample_uniform/3,draw_uniform/2,matrixproduct/4,optimalproposal/7,poisson/2,student/2,studentPdf/3,gamma/3,normal/3,normal2/6,densityGaussian/4,dirichlet/2,gaussian/3,kalman/13,kalmanrao/14,kalmanrao_simplified/9]).

:- load_foreign_files(['sampling'],[],init_my_predicates).

:- use_module(library(lists)).

draw_uniform([H|T],Element) :-
	length([H|T],Len),
	discreteuniform(Len,I), %I is integer(random*Len),
	nth0(I,[H|T],Element).

sample_uniform(A,B,X) :-
	number(A),
	number(B),
	A =< B,
	X is random*(B-A)+A.
	
% Bayesian Dirichlet score
dirichlet_score(Alpha,Beta,Pos,Neg,V) :-
	NewP is Pos+Alpha,
	NewN is Neg+Beta,
	betafunction(NewP,NewN,Num),
	betafunction(Alpha,Beta,Den),
	V is Num/Den.

sumvector([],[],[]) :-
	!.
	
sumvector([H1|T1],[H2|T2],[H|T]) :-
	sumvector(T1,T2,T),
	H is H1+H2,!.
		
% Log Bayesian Dirichlet score
logdirichlet_score(A,Examples,V) :-
	sumvector(A,Examples,Res),
	logbetafunction(Res,Num),
	logbetafunction(A,Den),
	V is Num-Den.