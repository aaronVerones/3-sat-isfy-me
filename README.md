# 3-sat-isfy-me

USEAGE: sat(clauses, variables)
clauses: [literal]+
variables: [variable]+
literal: polarity-variable
polarity: true | false
variable: [A-Z]+
EXAMPLE: sat([[true-X,false-Y],[false-X,false-Y],[true-X,true-Z]],[X,Y,Z]).

This is an implementation of the Davis, Putnam, Logemann, Loveland (DPLL) algorithm
for the boolean satisfiability problem using a mechanism called "watched literals".
This algorithm is special because it falls perfectly naturally into the logic programming paradigm
taking advantage of Prolog's backtracking search mechanism and declaritive syntax.

The algorithm is described and code is given in Howe & King's paper "A pearl on SAT and SMT solving in Prolog."
This is our implementation of their algorithm in SWI Prolog.

For the "Something Extra", you'll want to take a look at the following files:

1. sat_to_3sat.pl, where we take an arbitrary SAT input and reduce it to 3-SAT.
2. graph_colour.pl, where we solve Graph Colouring in Prolog.
3. subset_sum.pl, where we solve Subset Sum in Prolog.

REFERENCE:

Howe, J. M., & King, A. (2012). A pearl on SAT and SMT solving in Prolog.
Theoretical Computer Science, 435, 43–55. doi: 10.1016/j.tcs.2012.02.024
