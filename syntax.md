**DC Syntax (work in progress)**

***deterministic clauses***

Comments as in Prolog: %

Syntax: head := body.
Explaination: body implies head as clauses in prolog, the only difference is the implication operator := instead of :-

Example:
```
head(X) := body(X,Y).

above(1,2) := true. % the body has to be specified to distinguish with prolog syntax
```

***Distributional clauses***

Syntax: head ~ distribution := body.

Explaination: if 'body' is true the random variable head is defined with a given distribution
If none of the body that define head is true, the random variable head is not defined 

Examples:
```
coin ~ finite([0.2:true,0.8:false]). % the body is implicitly true.
coin2 ~ finite([0.2:true,0.8:false]) := coin ~= true.
```

