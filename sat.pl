%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEAGE: sat(clauses, variables)
%   clauses: [literal]+
%   variables: [variable]+
%   literal: polarity-variable
%   polarity: true | false
%   variable: [A-Z]+
% EXAMPLE: sat([[true-X,false-Y],[false-X,false-Y],[true-X,true-Z]],[X,Y,Z]).
%
% This is an implementation of the Davis, Putnam, Logemann, Loveland (DPLL) algorithm
% for the boolean satisfiability problem using a mechanism called "watched literals".
% This algorithm is special because it falls perfectly naturally into the logic programming paradigm
% taking advantage of Prolog's backtracking search mechanism and declaritive syntax.
%
% The algorithm is described and code is given in Howe & King's paper "A pearl on SAT and SMT solving in Prolog."
% This is our implementation of their algorithm in SWI Prolog.
%
% REFERENCE:
%
% Howe, J. M., & King, A. (2012). A pearl on SAT and SMT solving in Prolog.
%   Theoretical Computer Science, 435, 43–55. doi: 10.1016/j.tcs.2012.02.024
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(sat, [sat/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are trivial steps that set up the algorithm %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solves SAT
sat(Clauses, Variables) :-
  isSatisfiable(Clauses), bindToTrueFalse(Variables).

% binds all provided variables to 'true' or 'false' - otherwise they could be any value
bindToTrueFalse([]).
bindToTrueFalse([V|Rest]) :-
  (V = true ; V = false),
  bindToTrueFalse(Rest).

% an empty set of clauses is vacuously satisfiable
isSatisfiable([]).
% a formula is satisfiable if all clauses are satisfiable together
isSatisfiable([C|Rest]) :-
  satisfyClause(C),
  isSatisfiable(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the actual meat of the algorithm %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start watching each variable. Polarity is whether the literal X was declared as X or ~X in the calling of the function.
% (true-X, false-X)
satisfyClause([Polarity-Variable | Literals]) :-
  setupWatch(Literals, Variable, Polarity).

% base case: if we're at the end of the clause, try greedily making the clause true by setting the variable to its polarity. e.g. if the variable was provided as ~X, set it to false.
setupWatch([], Variable, Polarity) :- Variable = Polarity.
% recursive case: Set a watch on each pair of variables in the clause
setupWatch([P2-V2 | Literals], V1, P1) :-
    when(
      (nonvar(V1) ; nonvar(V2)), % nonvar is used to check whether a variable has already been assigned a truth value
      watch(V1, P1, V2, P2, Literals) % if at least one of V1 or V2 are still free, set a watch on them
    ).

% Set a watch on two variables. A watch evaluates to true when
watch(V1, P1, V2, P2, Literals) :-
  nonvar(V1) ->
    updateWatch(V1, P1, V2, P2, Literals); % if the variable in the first literal is free, continue to set up watches.
    updateWatch(V2, P2, V1, P1, Literals). % else, this means we're on a backtracking step.

% check if a watch on two literals is true; if not, set up a watch on the next two literals.
updateWatch(V1, P1, V2, P2, Literals) :-
    literalIsTrue(V1, P1); % if an literal is true, the clause is satisfied
    setupWatch(Literals, V2, P2). % if we can't make it true, we need to watch the next two literals

% checks if a sat literal is true. ~X (false-X) is true when X is false, X (true-X) is true when X is true.
% an literal is thus true when it equals its polarity.
literalIsTrue(V, P) :- V == P.
