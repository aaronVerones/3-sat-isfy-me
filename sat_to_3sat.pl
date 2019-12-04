%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEAGE: sat_to_3sat(clauses, variables, RC, RV)
%   clauses: [literal]+
%   variables: [variable]+
%   literal: polarity-variable
%   polarity: true | false
%   variable: [A-Z]+
% EXAMPLE: sat_to_3sat([[true-x,false-y],[false-x,false-y,true-a],[true-x,true-z,false-a,true-x,false-y]],[a,x,y,z], RV, RC).
%
% sat_to_3sat will convert any arbitrary SAT input to a 3-SAT input.
% This procedure it particularly useful when doing NP-Complete reductions, as 3SAT has predictable form.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(reductions, [sat_to_3sat/4]).

:- use_module(sat, [sat/2]).
:- use_module(library(dummy)).

% Converts SAT to 3-SAT. Note that there are two outputs: Clauses and Variables.
sat_to_3sat([], Variables, [], Variables).
sat_to_3sat([Clause | Tail], Variables, ResultClauses, ResultVariables) :-
  threeify_clause(Clause, Variables, ThreeClause, ThreeVariables),
  sat_to_3sat(Tail, ThreeVariables, Rest, ResultVariables),
  append(ThreeClause, Rest, ResultClauses).

% Given a clause in disjunctive normal form, convert it
% into a series of 3-satisfiable clauses with dummy variables.
threeify_clause([], Variables, [], Variables).
threeify_clause([C], Variables, [[C, C, C]], Variables).
threeify_clause([C1, C2], Variables, [[C1, C1, C2]], Variables).
threeify_clause([C1, C2, C3], Variables, [[C1, C2, C3]], Variables).
threeify_clause([C1, C2, C3, C4 | Rest], Variables, [[C1, C2, true-DummyVar] | Result], NewVariables) :-
  generate_dummy(Variables, DummyVar),
  threeify_clause([false-DummyVar, C3, C4 | Rest], [DummyVar | Variables], Result, NewVariables).
