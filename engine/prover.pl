:- module(prover, [
    prove_yn/1,
    answer_who/2,
    answer_what/2,
    prove_drs/1,
    prove_fol/1,
    bootstrap/0
]).
:- use_module(kb/facts).

bootstrap :-
    forall(facts:fact(F), assertz(F)),
    forall(facts:world_rule(R), assertz(R)).

% ============================================
% THEOREM PROVER FOR DRS/FOL
% ============================================

% Prove a DRS by finding bindings for discourse referents
% that satisfy all conditions
prove_drs(drs([], Conditions)) :- !,
    % No discourse referents, just prove all conditions
    prove_conditions(Conditions).

prove_drs(drs(Universe, Conditions)) :-
    % Find bindings for all variables in Universe
    % such that all Conditions are satisfied
    prove_conditions(Conditions).

% Prove all conditions in a list
prove_conditions([]) :- !.
prove_conditions([Cond|Rest]) :-
    prove_single_condition(Cond),
    prove_conditions(Rest).

% Prove a single condition
prove_single_condition(conj(Conditions)) :- !,
    % Conjunction - prove all conditions in the list
    prove_conditions(Conditions).

prove_single_condition(type(Entity, Type)) :- !,
    facts:type(Entity, Type).

prove_single_condition(Pred) :-
    % Try to prove using facts - wrap in fact/1
    facts:fact(Pred).

% ============================================
% FOL THEOREM PROVER
% ============================================

% Prove First-Order Logic formula
prove_fol(true) :- !.

prove_fol(and(P, Q)) :- !,
    prove_fol(P),
    prove_fol(Q).

prove_fol(and(Conditions)) :- is_list(Conditions), !,
    maplist(prove_fol, Conditions).

prove_fol(or(P, Q)) :- !,
    (prove_fol(P) ; prove_fol(Q)).

prove_fol(not(P)) :- !,
    \+ prove_fol(P).

prove_fol(implies(P, Q)) :- !,
    (prove_fol(P) -> prove_fol(Q) ; true).

prove_fol(exists(Var, Body)) :- !,
    % Existential: find at least one binding for Var
    prove_fol(Body).

prove_fol(forall(Var, Body)) :- !,
    % Universal: Body must hold for all values
    % This is complex - simplified version
    \+ (prove_fol(not(Body))).

prove_fol(type(Entity, Type)) :- !,
    facts:type(Entity, Type).

prove_fol(Pred) :-
    % Atomic predicate
    call(Pred).

% ============================================
% BACKWARD COMPATIBILITY
% ============================================

% Yes/No - now handles both old style and DRS
prove_yn(drs(Universe, Conditions)) :- !,
    prove_drs(drs(Universe, Conditions)).

prove_yn(conj(Cs)) :- !,
    maplist(call, Cs).

prove_yn(Prop) :- !,
    call(Prop).

% ============================================
% WH-QUESTION ANSWERING
% ============================================

% Answer "who" questions using DRS
answer_who(drs(Universe, Conditions), Answers) :-
    !,
    % Find all bindings for the first variable in Universe
    Universe = [Var|_],
    findall(Var, prove_conditions(Conditions), Bag),
    sort(Bag, Answers).

% Answer "what" questions using DRS
answer_what(drs(Universe, Conditions), Answers) :-
    !,
    % Find all bindings for the first variable in Universe
    Universe = [Var|_],
    findall(Var, prove_conditions(Conditions), Bag),
    sort(Bag, Answers).

% Old-style who/what (for backward compatibility)
answer_who(PropTemplate, Answers) :-
    \+ functor(PropTemplate, drs, 2),
    arg(1, PropTemplate, X),
    findall(X, (call(PropTemplate), facts:type(X, nguoi)), Bag),
    sort(Bag, Answers).

answer_what(RelTemplate, Answers) :-
    \+ functor(RelTemplate, drs, 2),
    arg(2, RelTemplate, Y),
    findall(Y, call(RelTemplate), Bag0),
    sort(Bag0, Answers).
