%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).
builtin(geometric(V)).

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

test_prodgaussian :-
	generate_backward(prod ~= V,L),
	write('product gaussians: '),writeln(V),
	writeln('(partial) generated sample:'),writeln(L).
	
	
% trick coin (define trick-coin (lambda () (if (flip 0.95) 'h 't)))
% (hist (repeat 20 trick-coin) "trick coin")
trickcoin ~ finite([0.95:h,0.05:t]).
test_trickcoin(N) :-
	init,
	eval_query([],[],trickcoin ~= h,N,P,_,_), % compute probability coin=true
	write('probability coin=true: '),writeln(P).


/* Bend coin example in Church
(define (make-coin weight) (lambda () (if (flip weight) 'h 't)))
(define (bend coin)
  (lambda () (if (equal? (coin) 'h)
                 ( (make-coin 0.7) )
                 ( (make-coin 0.1) ) )))

(define fair-coin (make-coin 0.5))
(define bent-coin (bend fair-coin))

(hist (repeat 100 bent-coin) "bent coin")
*/
fair-coin ~ finite([0.5:h,0.5:t]).
bent-coin ~ finite([0.7:h,0.3:t]) :=
	fair-coin ~= h.
bent-coin ~ finite([0.1:h,0.9:t]) :=
	fair-coin ~= t.
test_bendcoin(N) :-
	init,
	eval_query([],[],bent-coin ~= h,N,P,_,_),
	write('probability bent-coin = h: '),writeln(P).
	
	
% Medical example in Church
/*
(define (lung-cancer person)  (flip 0.01))
(define (cold person)  (flip 0.2))

(define (cough person) (or (cold person) (lung-cancer person)))

(list  (cough 'bob) (cough 'alice))
*/

lung-cancer(Person) ~ finite([0.01:true,0.99:false]).
cold(Person) ~ finite([0.2:true,0.8:false]).
cough(Person) :=
	 lung-cancer(Person) ~= true.
cough(Person) :=
	 cold(Person) ~= true.

test_med(N) :-
	init,
	eval_query([],[],cough(bob),N,P,_,_),
	write('probability cough(bob): '),writeln(P).
	
% geometric distribution
flip(Depth) ~ finite([0.5:true,0.5:false]).
geometric(0,D) :=
	flip(D) ~= true.
geometric(V,D) :=
	flip(D) ~= false,
	geometric(V2,next(D)),
	V is 1+V2.

test_geometric :-
	generate_backward(geometric(V,0),L),
%	generate_backward(geometric2 ~= _,L2),
	write('geometric: '),writeln(V),
	writeln('(partial) generated sample:'),writeln(L).



% alternative geometric (requires tabling)
flip1(_) ~ finite([0.1:true,0.9:false]).

c(0) := flip1(0)~=false.
c(N) := 
	c(P),
	N is P+1,
	flip1(N)~=false.
	
stop(0) := flip1(0)~=true.
stop(N) :=
	c(P),
	N is P+1,
	flip1(N)~=true.

test_geometric2 :-
	generate_backward(stop(V),L),
%	generate_backward(geometric2 ~= _,L2),
	write('geometric: '),writeln(V),
	writeln('(partial) generated sample:'),writeln(L).


% alternative (inductive) definition of a geometric distribution
flip2(0) ~ finite([0.1:true,0.9:false]).
flip2(B) ~ finite([0.1:true,0.9:false]) :=
	flip2(A)~=false,
	B is A+1.

geo(V) :=
	flip2(V) ~= true.


test_geometric3 :-
	generate_backward(geo(V),L),
%	generate_backward(geometric2 ~= _,L2),
	write('geometric: '),writeln(V),
	writeln('(partial) generated sample:'),writeln(L).

geometric2 ~ val(V) := geometric(V).
% prolog version
geometric(V) :-
	(true(0.5) ->
		V=0
		;
		(
			geometric(V2),
			V is V2+1
		)
	).

/*

(define (take-sample)
  (rejection-query

   (define A (if (flip) 1 0))
   (define B (if (flip) 1 0))
   (define C (if (flip) 1 0))
   (define D (+ A B C))

   A

   (condition (>= D 2))))

(hist (repeat 100 take-sample) "Value of A, given that D is greater than or equal to 2")
*/
a ~ finite([0.1:1,0.9:0]).
b ~ finite([0.1:1,0.9:0]).
c ~ finite([0.1:1,0.9:0]).
d ~ val(D) :=
	a~=A,b~=B,c~=C,D is A+B+C.
test_conditional(N) :-
	init,
	%eval_query([(d~=D,D>=2)],[],a ~= 1,N,P,_,_),
	eval_query_distribution(X,[(d~=D,D>=2)],[],a ~= X,N,LP,_,_),
	%eval_query_distribution_eval(X,[(d~=D,D>=2)],[],a ~= X,N,LP2,_,_),
	write('distribution a: '),writeln(LP),writeln(LP2).


aaa ~ finite([0.1:1,0.9:0]).
bbb ~ finite([0.1:A,0.9:B]) :=
	aaa~=A,
	B is 1-A.
ccc ~ finite([0.4:A,0.6:B]) :=
	bbb~=A,
	B is 1-A.
prova(1) ~ finite([1:1]).
prova(2) ~ finite([1:1]).
prova2(1) ~ finite([0.01:1,0.99:0]).
prova2(2) ~ finite([1:1]).

%init,eval_query_backward([],[],(bbb ~= A,aaa~= A,ccc~=A),10000,P,Succ_Sum,Sum).
%init,eval_query([],[],(aaa~= A,bbb~= A,ccc~=A),100,P,_,_).
%init,distributionalclause:eval_query_backward_lw([],[],(bbb ~= A,aaa~= A,ccc~=A),100,P,Succ_Sum,Sum).
%init,distributionalclause:eval_query_backward_lw([],[],(prova(X) ~= A,bbb ~= A,aaa~= A,ccc~=A),100,P,Succ_Sum,Sum).
%init,eval_query([],[],(prova(X)~= 1,prova2(X)~= 1),100,P,_,_).
%init,distributionalclause:eval_query_backward_lw([],[],(prova(X)~= 1,prova2(X)~= 1),100,P,Succ_Sum,Sum).
/*
breast cancer example in Church
(define samples
 (mh-query 100 100
   (define breast-cancer (flip 0.01))

   (define positive-mammogram (if breast-cancer (flip 0.8) (flip 0.096)))

   breast-cancer

   positive-mammogram
 )
)
(hist samples "breast cancer")
*/
breast-cancer ~ finite([0.01:true,0.99:false]).
positive-mammogram ~ finite([0.8:true,0.2:false]) :=
	breast-cancer ~= true.
positive-mammogram ~ finite([0.096:true,0.904:false]) :=
	breast-cancer ~= false.
	
test_breastcancer(N) :-
	init,
	eval_query([positive-mammogram~=true],[],breast-cancer ~= true,N,P,_,_),
	write('prob breast-cancer: '),writeln(P).
	
/*
(define samples
  (mh-query 1000 100
    (define lung-cancer (flip 0.01))
    (define TB (flip 0.005))
    (define cold (flip 0.2))
    (define stomach-flu (flip 0.1))
    (define other (flip 0.1))

    (define cough (or (and cold (flip 0.5)) (and lung-cancer (flip 0.3)) (and TB (flip 0.7)) (and other (flip 0.01))))
    (define fever (or (and cold (flip 0.3)) (and stomach-flu (flip 0.5)) (and TB (flip 0.2)) (and other (flip 0.01))))
    (define chest-pain (or (and lung-cancer (flip 0.4)) (and TB (flip 0.5)) (and other( flip 0.01))))
    (define shortness-of-breath (or (and lung-cancer (flip 0.4)) (and TB (flip 0.5)) (and other (flip 0.01))))

    (list lung-cancer TB)

    (and cough fever chest-pain shortness-of-breath)

  )
)
(hist samples "Joint inferences for lung cancer and TB")

*/
lung_cancer ~ finite([0.01:true,0.99:false]).
tb ~ finite([0.005:true,0.995:false]).
cold  ~ finite([0.2:true,0.8:false]).
stomach_flu ~ finite([0.1:true,0.9:false]).
other ~ finite([0.1:true,0.9:false]).

cough :=
	cold ~= true,true(0.5).
cough :=
	lung_cancer ~= true,true(0.3).
cough :=
	tb ~= true,true(0.7).
cough :=
	other ~= true,true(0.01).

fever :=
	cold ~= true,true(0.3).
fever :=
	stomach_flu ~= true,true(0.5).
fever :=
	tb ~= true,true(0.2).
fever :=
	other ~= true,true(0.01).

chest_pain :=
	lung_cancer ~= true,true(0.4).
chest_pain :=
	tb ~= true,true(0.5).
chest_pain :=
	other ~= true,true(0.01).

shortness_of_breath :=
	lung_cancer ~= true,true(0.4).
shortness_of_breath :=
	tb ~= true,true(0.5).
shortness_of_breath :=
	other ~= true,true(0.01).

test_med2(N) :-
	init,
	query([cough, fever, chest_pain, shortness_of_breath],[],lung_cancer ~= true,N,P,_,_),
	write('prob lung_cancer: '),writeln(P),
	query([cough, fever, chest_pain, shortness_of_breath],[],(lung_cancer ~= true,tb~=true),N,P2,_,_),
	write('prob lung_cancer and tb: '),writeln(P2),
	eval_query_distribution((X,Y),[cough, fever, chest_pain, shortness_of_breath],[],(lung_cancer ~= X,tb~=Y),N,P3,_,_),
%	eval_query_distribution_eval((X,Y),[cough, fever, chest_pain, shortness_of_breath],[],(lung_cancer ~= X,tb~=Y),N,P4,_,_),
	write('prob lung_cancer,tb: '),writeln(P3).
	