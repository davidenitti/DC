%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)),set_query_propagation(true).
%:- set_debug(true).

:- initialization(time(test_gpa(10000))).

coin ~ finite([0.95:true,0.05:false]).
agpa ~ beta(8,2) := coin~=true.
american_gpa ~ finite([0.85:4.0,0.15:0.0]) := coin~=false.

american_gpa ~ val(V) := agpa ~=A, V is A*4.0.

coin2 ~ finite([0.99:true,0.01:false]).
igpa ~ beta(5,5) := coin2~=true.
indian_gpa ~ finite([0.1:0.0,0.9:10.0]) := coin2~=false.

indian_gpa ~ val(V) := igpa ~=A, V is A*10.0.

nation ~ finite([0.25:a,0.75:i]).

student_gpa ~ val(A) := nation~=a,american_gpa~=A.
student_gpa ~ val(I) := nation~=i,indian_gpa~=I.
/*
y(V) := a~=V,x(V).
y(V) := b~=V,x(V).

x(X) := X>0.
z(X) := X>1.
a ~ finite([0.25:1,0.75:2]).
b ~ finite([0.25:1,0.75:2]).
c := y(V),z(V).
test1(N) :-
	init,
	eval_query([],[],c,N,P,A,B), % p(nation~=a|student_gpa~=3.9)
	write('probability: '),writeln((P,A,B)).
*/
%evid := student_gpa~=3.9.

% p(nation~=a|student_gpa~=3)=0.3854718517966607
% p(nation~=a|student_gpa~=3.9)=0.1928808855689747
% p(nation~=a|student_gpa~=4)=1.0


	
% N is the number of samples
test_gpa(N) :-
	init,
	eval_query([student_gpa~=3.9],[],nation~=a,N,P,A,B), % p(nation~=a|student_gpa~=3.9)
	write('probability: '),writeln((P,A,B)).

	
e1_lw(N) :-
	init,
	File='gpa.txt',
	Q=[nation~=a],
	E=[student_gpa~=4],
	distributionalclause:eval_query_backward_lw(E,[],Q,1,N,P1,_,_),writeln(P1),halt.
	
e2_lw(N) :-
	init,
	File='gpa.txt',
	Q=[nation~=a],
	E=[student_gpa~=3.9],
	distributionalclause:eval_query_backward_lw(E,[],Q,1,N,P1,_,_),writeln(P1),halt.

e1(Runs) :-
	init,
	File='gpa.txt',
	Q=[nation~=a],
	E=[student_gpa~=4],
	experiment_LW(File,Q,E,50,Runs,_,_,_),
	experiment_LW(File,Q,E,100,Runs,_,_,_),
	experiment_LW(File,Q,E,200,Runs,_,_,_),
	experiment_LW(File,Q,E,500,Runs,_,_,_),
	experiment_LW(File,Q,E,1000,Runs,_,_,_),
	experiment_LW(File,Q,E,2000,Runs,_,_,_),
	experiment_LW(File,Q,E,5000,Runs,_,_,_),
	experiment_LW(File,Q,E,10000,Runs,_,_,_),
	experiment_LW(File,Q,E,20000,Runs,_,_,_),
	experiment_LW(File,Q,E,50000,Runs,_,_,_),
	experiment_LW(File,Q,E,100000,Runs,_,_,_).
	
e2(Runs) :-
	init,
	File='gpa2.txt',
	Q=[nation~=a],
	E=[student_gpa~=3.9],
	experiment_LW(File,Q,E,50,Runs,_,_,_),
	experiment_LW(File,Q,E,100,Runs,_,_,_),
	experiment_LW(File,Q,E,200,Runs,_,_,_),
	experiment_LW(File,Q,E,500,Runs,_,_,_),
	experiment_LW(File,Q,E,1000,Runs,_,_,_),
	experiment_LW(File,Q,E,2000,Runs,_,_,_),
	experiment_LW(File,Q,E,5000,Runs,_,_,_),
	experiment_LW(File,Q,E,10000,Runs,_,_,_),
	experiment_LW(File,Q,E,20000,Runs,_,_,_),
	experiment_LW(File,Q,E,50000,Runs,_,_,_),
	experiment_LW(File,Q,E,100000,Runs,_,_,_).
