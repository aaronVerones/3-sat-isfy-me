% This algorithm is cool because we just have to tell the program what makes a clause true and prolog will find a satisfying truth assignment using a backtracking search procedure that mirrors the search procedure of the DPLL algorithm

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the actual meat of the algorithm %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% checks if a sat literal is true. ~X (false-X) is true when X is false, X (true-X) is true when X is true.
% an literal is thus true when it equals its polarity.
literalIsTrue(V, P) :- V == P.

% check if a watch on two literals is true; if not, set up a watch on the next two literals.
updateWatch(V1, P1, V2, P2, Literals) :- 
    literalIsTrue(V1, P1); % if an literal is true, the clause is satisfied
    setupWatch(Literals, V2, P2). % if we can't make it true, we need to watch the next two literals

% Set a watch on two variables. A watch evaluates to true when 
watch(V1, P1, V2, P2, Literals) :-
  nonvar(V1) -> 
    updateWatch(V1, P1, V2, P2, Literals); % if the variable in the first literal is free, continue to set up watches.
    updateWatch(V2, P2, V1, P1, Literals). % else, this means we're on a backtracking step.

% base case: if we're at the end of the clause, try greedily making the clause true by setting the variable to its polarity. e.g. if the variable was provided as ~X, set it to false. 
setupWatch([], Variable, Polarity) :- Variable = Polarity. 
% recursive case: Set a watch on each pair of variables in the clause
setupWatch([P2-V2 | Literals], V1, P1) :- 
    when( 
      (nonvar(V1) ; nonvar(V2)), % nonvar is used to check whether a variable has already been assigned a truth value
      watch(V1, P1, V2, P2, Literals) % if at least one of V1 or V2 are still free, set a watch on them
    ). 

% start watching each variable. Polarity is whether the literal X was declared as X or ~X in the calling of the function. (true-X, false-X)
satisfyClause([Polarity-Variable | Literals]) :-
  setupWatch(Literals, Variable, Polarity). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are trivial steps that set up the algorithm %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% an empty set of clauses is vacuously satisfiable
isSatisfiable([]).
% a formula is satisfiable if all clauses are satisfiable together
isSatisfiable([C|Rest]) :-
  satisfyClause(C),
  isSatisfiable(Rest).

% binds all provided variables to 'true' or 'false' - otherwise they could be any value
bindToTrueFalse([]).
bindToTrueFalse([V|Rest]) :-
  (V = true ; V = false),
  bindToTrueFalse(Rest).

% solves SAT
sat(Clauses, Variables) :-
  isSatisfiable(Clauses), bindToTrueFalse(Variables).
