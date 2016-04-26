%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl'). % load distributional clauses library
:- use_module('../random/sampling.pl'). % load sampling library
:- use_module(library(lists)). % load list library (prolog)

:- set_options(default),set_inference(backward(lw)). % to enable query propagation replace the line with :- set_options(default),set_inference(backward(lw)),set_query_propagation(true).
%:- set_debug(true).


:- initialization(init). % initialize DC

% define a discrete random variable (bernoulli)
coin ~ finite([0.2:true,0.8:false]). % syntax name_variable ~ finite([prob:value,prob:value,...])

% call for example test_coin(100). to use 100 samples
test_coin(N) :-
	init, % initialize DC
	% syntax query: positive evidence,negative evidence, query, number of samples, result (probability of the query)
	query([],[],coin ~= true,N,P), % compute probability coin=true, use name_var~=value to compare the value of random variable with a value
	write('probability coin=true: '),writeln(P). % output the result

% define another discrete random variable (categorical), same sintax as before
color ~ finite([0.1:black,0.5:red,0.4:blue]). % syntax name_variable ~ finite([prob:value,prob:value,...])

% call for example test_color(100). to use 100 samples
test_color(N) :-
	init, % initialize DC
	% syntax query: positive evidence,negative evidence, query, number of samples, result (probability of the query)
	query([],[],color ~= black,N,P), % compute probability color=black, use name_var~=value to compare the value of random variable with a value
	write('probability color=black: '),writeln(P). % output the result

% define another discrete random variable (categorical) with uniform distribution
nballs ~ uniform([1,2,3,4,5,6,7,8,9,10]).


% call for example test_balls(100). to use 100 samples
test_balls(N) :-
	init, % initialize DC
	% syntax query: positive evidence,negative evidence, query, number of samples, result (probability of the query)
	query([],[],nballs ~= 4,N,P), % compute probability color=black, use name_var~=value to compare the value of random variable with a value
	query([],[],(nballs ~= NB,NB>5),N,P2), % compute probability nballs>5, get first the value of nballs in the logical variable NB, then compare NB with 5
	write('probability nballs=4: '),writeln(P), % output the result
	write('probability nballs>5: '),writeln(P2). % output the result

% deterministic rule to define facts given conditions
ball(X) := nballs ~= N, between(1,N,X). % read as: for each X and N, if nballs=N and X is between 1 and N then ball(X) is true. nballs=N will succeed for N equals to the value of the number of balls. For example, if nballs=3 then ball(1),ball(2),ball(3) are true.

% define a random variable for each ball
material(X) ~ finite([0.3:wood,0.7:metal]) := ball(X). % read as: for each X if ball(X) is true then the random variable material(X) has a given distribution. For example, if ball(1) and ball(2) are true, then material(1) and material(2) are 2 i.i.d. random variables.

% define the color of each ball: the color distribution depends on the material.
color(X) ~ uniform([grey,blue,black]) := material(X) ~= metal. % read as: for each X if the material(X) is metal then color(X) has a given uniform distribution.
color(X) ~ uniform([black,brown]) := material(X) ~= wood. % read as: for each X if the material(X) is wood then color(X) has a given uniform distribution (different from the previous one).

% define draws with replacement. The ball drawn has a uniform distribution over the number of balls. However, the number of balls is a random variable itself.
drawn(Draw) ~ uniform(Balls) := nballs ~= N, findall(X,between(1,N,X),Balls).

% define the size of each ball with a beta distribution. The size distribution depends on the material
size(X) ~ beta(2,3) := material(X) ~= metal.
size(X) ~ beta(4,2) := material(X) ~= wood.

% call for example test_balls2(1000). to use 1000 samples
test_balls2(N) :-
	init, % initialize DC
	% syntax query: positive evidence,negative evidence, query, number of samples, result (probability of the query)
	query([],[],(drawn(1) ~= X,color(X)~=black),N,P), % compute probability ball drawn number 1 has color red
	write('probability color first drawn ball = red: '),writeln(P). % output the result
