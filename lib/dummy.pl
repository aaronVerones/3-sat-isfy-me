% Generates dummy constants based on existing constants.
:- module(dummy, [generate_dummy/2]).

% Generates dummy contants unique from the given constants.
generate_dummy(Constants, dummy-N) :-
  number(0, N),
  unique_constant(Constants, dummy-N).

% Numbers to choose for a dummy constant.
number(Start, Start).
number(Start, Result) :-
  Value is Start + 1,
  number(Value, Result).

% Chooses an element from a list.
choose([H | T], H).
choose([H | T], R) :- choose(T, R).

% Determines if a contant is unique in a list.
unique_constant([], _).
unique_constant([H | T], Var) :- dif(Var, H), unique_constant(T, Var).
