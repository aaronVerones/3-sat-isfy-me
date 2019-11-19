watch(V1, P1, V2, P2, Rest) :-
  nonvar(V1) ->
    % if V1 is free, update 1
    updateWatch(V1, P1, V2, P2, Rest);

% base case: if we're at the end of the clause, try greedily setting the variable to its polarity. e.g. if the variable was provided as ~X, set it to false.
setupWatch(Polarity, Variable, []) :- Variable = Polarity.
setupWatch(Polarity, Variable, [P1-V1 | Rest]) :-
  when(
    (nonvar(Variable) ; nonvar(V1)),
    watch(Variable, Polarity, V2, P2, Rest)
  ).

% start watch on each variable. Polarity is whether the variable was declared as X or ~X in the calling of the function. (true-X, false-X)
satisfyClause([Polarity-Variable] | Rest) :-
  setupWatch(Polarity, Variable, Rest).

% an empty set of clauses is vacuously satisfiable
isSatisfiable([]).
% a formula is satisfiable if all clauses are satisfiable together
isSatisfiable([C|Rest]) :-
  satisfyClause(C),
  isSatisfiable(Rest).

% binds all provided variables to 'true' or 'false' for readability
bindToTrueFalse([]).
bindToTrueFalse([V|Rest]) :-
  (V = true ; V = false),
  bindToTrueFalse(Rest).

% solves SAT
sat(Clauses, Variables) :-
  isSatisfiable(Clauses), bindToTrueFalse(Variables).