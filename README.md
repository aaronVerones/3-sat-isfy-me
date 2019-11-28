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

REFERENCE:

Howe, J. M., & King, A. (2012). A pearl on SAT and SMT solving in Prolog.
Theoretical Computer Science, 435, 43–55. doi: 10.1016/j.tcs.2012.02.024
