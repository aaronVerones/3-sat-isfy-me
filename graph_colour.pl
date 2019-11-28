%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEAGE: graph_colour(nodes, edges, numColours, Result)
%   nodes: [node(, node)*]
%   edges: [edge(, edge)*]
%   edge:  [node, node]
%   node:  (a-z)+(0-9)*
%   numColours: (1-9)(0-9)*
% Result will return the assignments of nodes to colours in a map.
% EXAMPLE: graph_colour([n1, n2, n3, n4, n5, n6], [[n1, n3], [n2, n3], [n3, n4], [n3, n5], [n4, n6], [n5, n6]], 2, R).
%
% Graph Colour is one of the most famous NP-Complete problems. It's also one of the easiest to understand.
% Given a description of a graph G=<V,E>, and the number of colours numColours, determine if the graph can
% be coloured using only those numColours, where nodes connected by an edge cannot have the same colour.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(graph_colour, [graph_colour/4]).
:- use_module(library(map)).

% Solves graph colouring.
graph_colour(Nodes, Edges, NumColours, Assignments) :-
  assign_colours_to_nodes(Nodes, Edges, NumColours, map([]), Assignments),
  all_nodes_assigned(Nodes, Assignments),
  edges_coloured_different(Nodes, Edges, Assignments).

% Assigns colours to each node one of NumColours.
assign_colours_to_nodes([], _, _, Seen, Seen).
assign_colours_to_nodes([Node | Rest], Edges, NumColours, Seen, Result) :-
  map_contains(Seen, Node),
  assign_colours_to_nodes(Rest, Edges, NumColours, Seen, Result).
assign_colours_to_nodes([Node | Rest], Edges, NumColours, Seen, Result) :-
  \+ map_contains(Seen, Node),
  edge_nodes(Node, Edges, EdgeNodes),
  map_lookup_multiple(Seen, EdgeNodes, ExcludedColours),
  assign_colour(NumColours, ExcludedColours, Colour),
  map_insert(Seen, Node, Colour, Assignments),
  assign_colours_to_nodes(Rest, Edges, NumColours, Assignments, Result).

% Determines if all nodes have been assigned a colour.
all_nodes_assigned([], _).
all_nodes_assigned([Node | Rest], Assignments) :-
  map_contains(Assignments, Node),
  all_nodes_assigned(Rest, Assignments).

% Determines if nodes have conflicting colours.
edges_coloured_different([], _, _).
edges_coloured_different([Node | Rest], Edges, Assignments) :-
  map_lookup(Assignments, Node, Colour),
  edge_nodes(Node, Edges, EdgeNodes),
  nodes_not_coloured(Colour, EdgeNodes, Assignments),
  edges_coloured_different(Rest, Edges, Assignments).

% None of the nodes are assigned the given colour.
nodes_not_coloured(_, [], _).
nodes_not_coloured(Colour, [Node | Rest], Assignments) :-
  map_lookup(Assignments, Node, NodeColour),
  dif(Colour, NodeColour),
  nodes_not_coloured(Colour, Rest, Assignments).

% Returns the nodes that share an edge to the given node.
edge_nodes(_, [], []).
edge_nodes(Node, [[Node, Other] | Rest], R) :-
  edge_nodes(Node, Rest, EdgeNodes),
  insert(Other, EdgeNodes, R).
edge_nodes(Node, [[Other, Node] | Rest], R) :-
  edge_nodes(Node, Rest, EdgeNodes),
  insert(Other, EdgeNodes, R).
edge_nodes(Node, [[Other1, Other2] | Rest], R) :-
  dif(Node, Other1),
  dif(Node, Other2),
  edge_nodes(Node, Rest, R).

% Assigns a colour from NumColours given the colour is not excluded.
assign_colour(NumColours, [], Colour) :- colour(NumColours, Colour).
assign_colour(NumColours, [Excluded | Rest], Colour) :-
  dif(Colour, Excluded),
  assign_colour(NumColours, Rest, Colour).

% A colour is a number between 1 and the number of colours.
colour(NumColours, Colour) :- between(1, NumColours, Colour).

% Inserts an element into the list if it is unique.
insert(E, [], [E]).
insert(E, [E | T], [E | T]).
insert(E, [H | T], [H | R]) :- dif(E, H), insert(E, T, R).
