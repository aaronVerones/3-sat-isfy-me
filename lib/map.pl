:- module(map, [map/1, map_insert/4, map_lookup/3, map_lookup_multiple/3, map_contains/2 ]).

% A Map library to store Key,Value pairs.
% Not computationally efficient, but the interface is simple.
map([]).
map([[_, _] | _]).

% Inserts the key, value pair into the map.
% If the key, value pair exists, overwrite it.
map_insert(map([]), Key, Value, map([[Key, Value]])).
map_insert(map([[Key,  _] | R]), Key, Value, map([[Key, Value] | R])).
map_insert(map([[K1, V1] | R]), Key, Value, map([[K1, V1] | Result])) :-
  dif(K1, Key),
  map_insert(map(R), Key, Value, map(Result)).

% Looks up the key in the map and results its value.
map_lookup(map([[Key, Value] | _]), Key, Value).
map_lookup(map([[K1, _] | Rest]), Key, Value) :-
  dif(Key, K1),
  map_lookup(map(Rest), Key, Value).

% Looks up the values for multiple keys, returning a list of their results.
map_lookup_multiple(_, [], []).
map_lookup_multiple(Map, [ K1 | Rest], Result) :-
  \+ map_contains(Map, K1),
  map_lookup_multiple(Map, Rest, Result).
map_lookup_multiple(Map, [K1 | Rest], [V1 | Result]) :-
  map_lookup(Map, K1, V1),
  map_lookup_multiple(Map, Rest, Result).

% Determines if the given key is in the map.
map_contains(Map, Key) :- map_lookup(Map, Key, _).
