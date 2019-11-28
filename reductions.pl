%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USEAGE:
%   sat_via_graph_colour(clauses, variables)
%   sat_via_subset_sum(clauses, variables)
%   graph_colour_via_sat(nodes, edges, numColours)
%   graph_colour_via_subset_sum(nodes, edges, numColours)
%   subset_sum_via_sat(set, sum)
%   subset_sum_via_graph_colour(set, sum)
% See the respective algorithm files for the syntax of the arguments.
%
% We've implemented SAT, Graph Colouring, and Subset Sum in Prolog, but we're just getting started!
% Our next goal is the prove that these problems are actually the same problem.
% This demonstrates their NP-Completeness, since solving one will solve the rest.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(reductions, [
  sat_via_graph_colour/2,
  sat_via_subset_sum/2,
  graph_colour_via_sat/3,
  graph_colour_via_subset_sum/3,
  subset_sum_via_sat/2,
  subset_sum_via_sat/2
]).

:- use_module(sat).
:- use_module(graph_colour).
:- use_module(subset_sum).

% Reduces SAT to Graph Colour and solves.
% TODO:
sat_via_graph_colour(clauses, variables).

% Reduces SAT to Subset Sum and solves.
% TODO:
sat_via_subset_sum(clauses, variables).

% Reduces Graph Colour to SAT and solves.
% TODO:
graph_colour_via_sat(nodes, edges, numColours).

% Reduces Graph Colour to Subset Sum and solves.
% TODO:
graph_colour_via_subset_sum(nodes, edges, numColours).

% Reduces Subset Sum to Sat and solves.
% TODO:
subset_sum_via_sat(set, sum).

% Reduces Subset Sum to Graph Colour and solves.
% TODO:
subset_sum_via_graph_colour(set, sum).
