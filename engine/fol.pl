:- module(fol, [drs_to_fol/2, fol_simplify/2]).

% ============================================
% DRS TO FIRST-ORDER LOGIC CONVERSION
% ============================================

% Convert DRS to First-Order Logic
% drs([X, Y], [P(X), Q(X, Y)]) -> exists(X, exists(Y, P(X) & Q(X, Y)))

drs_to_fol(drs([], []), true) :- !.
% Empty DRS is trivially true

drs_to_fol(drs([], [Condition]), Condition) :- !.
% Single condition with no referents

drs_to_fol(drs([], Conditions), and(Conditions)) :- !.
% Multiple conditions with no referents

drs_to_fol(drs([Var], Conditions), exists(Var, FOL)) :- !,
    conditions_to_fol(Conditions, FOL).

drs_to_fol(drs([Var|Vars], Conditions), exists(Var, RestFOL)) :-
    drs_to_fol(drs(Vars, Conditions), RestFOL).

% Convert list of conditions to FOL conjunction
conditions_to_fol([], true).
conditions_to_fol([Cond], Cond) :- !.
conditions_to_fol([Cond|Conds], and(Cond, RestFOL)) :-
    conditions_to_fol(Conds, RestFOL).

% ============================================
% FOL SIMPLIFICATION
% ============================================

% Simplify FOL expressions for readability
fol_simplify(and(true, X), Simplified) :- !, fol_simplify(X, Simplified).
fol_simplify(and(X, true), Simplified) :- !, fol_simplify(X, Simplified).
fol_simplify(and(X, Y), and(XSimp, YSimp)) :- !,
    fol_simplify(X, XSimp),
    fol_simplify(Y, YSimp).

fol_simplify(exists(Var, Body), exists(Var, SimplifiedBody)) :- !,
    fol_simplify(Body, SimplifiedBody).

fol_simplify(or(X, Y), or(XSimp, YSimp)) :- !,
    fol_simplify(X, XSimp),
    fol_simplify(Y, YSimp).

fol_simplify(not(X), not(XSimp)) :- !,
    fol_simplify(X, XSimp).

fol_simplify(implies(X, Y), implies(XSimp, YSimp)) :- !,
    fol_simplify(X, XSimp),
    fol_simplify(Y, YSimp).

fol_simplify(X, X).
% Base case: atomic formula
