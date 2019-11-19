% DPLL Algorithm:
%
% (1)   function DPLL(f: CNF formula, θ : truth assignment)
% (2)   begin
% (3)     θ1 := θ ∪ unit-propagation(f, θ);
% (4)     if (is-satisfied(f, θ1)) then
% (5)       return θ1;
% (6)     else if (is-conflicting(f, θ1)) then
% (7)       return ⊥;
% (8)     endif
% (9)     x := choose-free-variable(f, θ1);
% (10)    θ2 := DPLL(f, θ1 ∪ {x 7→ true});
% (11)    if (θ2 6= ⊥) then
% (12)      return θ2;
% (13)    else
% (14)      return DPLL(f, θ1 ∪ {x 7→ f alse});
% (15)    endif
% (16)  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%A sat solver, utilising delay declaration to implement
%watched literals
%
%Version using when for delay (SWI)
%
%Authors: Jacob Howe and Andy King
%Last modified: 4/4/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(Clauses, Vars, Sat, _) :-
    sat(Clauses, Vars),
    !,
    Sat = true.
search(_Clauses, _Vars, false, _).

sat(Clauses, Vars) :-
    problem_setup(Clauses), elim_var(Vars).

elim_var([]). 
elim_var([Var | Vars]) :- 
    elim_var(Vars), (Var = true; Var = false). 

problem_setup([]). 
problem_setup([Clause | Clauses]) :- 
    clause_setup(Clause), 
    problem_setup(Clauses). 

clause_setup([Pol-Var | Pairs]) :- set_watch(Pairs, Var, Pol). 

set_watch([], Var, Pol) :- Var = Pol. 
set_watch([Pol2-Var2 | Pairs], Var1, Pol1) :- 
    when(;(nonvar(Var1),nonvar(Var2)),watch(Var1, Pol1, Var2, Pol2, Pairs)). 

watch(Var1, Pol1, Var2, Pol2, Pairs) :- 
    nonvar(Var1) -> 
      update_watch(Var1, Pol1, Var2, Pol2, Pairs); 
      update_watch(Var2, Pol2, Var1, Pol1, Pairs). 

update_watch(Var1, Pol1, Var2, Pol2, Pairs) :- 
    Var1 == Pol1 -> true; set_watch(Pairs, Var2, Pol2). 