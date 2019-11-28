%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEAGE: subset_sum(set, sum, Result)
%   set: [integer(, integer)*]
%   sum: integer
%   Integer: (1-9)(0-9)*
% Result will store the subset required to get the sum.
% EXAMPLE: subset_sum([3, 34, 4, 12, 5, 2], 9, R).
%
% Subset Sum is another famous NP-Complete problem, likely because the task sounds so simple!
% The goal is to find a subset of any length from the given set that add up to the sum.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(subset_sum, [subset_sum/3]).

% Solves Subset Sum
subset_sum(_, 0, []).
subset_sum(Set, Sum, Subset) :-
  create_subsets(Set, Subsets),
  try_element(Subsets, Subset),
  check_sum(Subset, Sum).

% Creates all subsets of a set.
create_subsets([], []).
create_subsets([Element | Rest], Result) :-
  create_subsets(Rest, Subsets),
  prepend_all(Element, Subsets, Prepends),
  concat(Prepends, Subsets, Result).

% Takes an element from the list.
try_element([Element | _], Element).
try_element([_ | Rest], Result) :- try_element(Rest, Result).

% Prepends the element to all elements of the list.
prepend_all(Element, [], [[Element]]).
prepend_all(Element, [H | T], [[Element | H] | Rest]) :-
  prepend_all(Element, T, Rest).

% Concatenates 2 lists into one.
concat([], L2, L2).
concat([H | T], L2, [H | Rest]) :- concat(T, L2, Rest).

% Checks that the sum of the subset equals the sum.
check_sum([], 0).
check_sum([Element | Rest], Sum) :-
  V is Sum - Element,
  V >= 0,
  check_sum(Rest, V).
