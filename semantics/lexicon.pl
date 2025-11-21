:- module(lexicon, [
    noun_sem/2,
    verb_iv_sem/2,
    verb_tv_sem/2,
    adj_sem/2,
    question_word_sem/2,
    apply/3,
    beta_reduce/2,
    sem_to_drs/2
]).

% ============================================
% LAMBDA EXPRESSIONS FOR LEXICAL SEMANTICS
% ============================================

% --- PROPER NOUNS (Danh từ riêng) ---
% Semantics: \P.P(entity)
% A proper noun takes a property P and applies it to the entity
noun_sem(huy, lam('P', app('P', huy))).
noun_sem(gau, lam('P', app('P', gau))).

% --- COMMON NOUNS (Danh từ chung) ---
% Semantics: \X.type(X, Type)
noun_sem(cho, lam('X', cho('X'))).
noun_sem(nguoi, lam('X', nguoi('X'))).

% --- INTRANSITIVE VERBS (Động từ nội) ---
% Semantics: \X.predicate(X)
verb_iv_sem(hien, lam('X', hien('X'))).
verb_iv_sem(nho, lam('X', nho('X'))).
verb_iv_sem(lon, lam('X', lon('X'))).

% --- TRANSITIVE VERBS (Động từ ngoại) ---
% Semantics: \Y.\X.predicate(X, Y)
% First takes the object (Y), then the subject (X)
verb_tv_sem(co, lam('Y', lam('X', co('X', 'Y')))).
verb_tv_sem(dat, lam('Y', lam('X', dat('X', 'Y')))).
verb_tv_sem(thich, lam('Y', lam('X', thich('X', 'Y')))).

% --- ADJECTIVES (Tính từ) ---
% Semantics: \X.property(X)
adj_sem(hien, lam('X', hien('X'))).
adj_sem(nho, lam('X', nho('X'))).

% --- QUESTION WORDS (Từ hỏi) ---
% "ai" (who): \P.drs([X], [type(X, nguoi), P(X)])
% Creates a DRS with a discourse referent X that is a person and has property P
question_word_sem(ai, lam('P', drs(['X'], [type('X', nguoi), app('P', 'X')]))).

% "gi" (what): \P.drs([X], [P(X)])
% Creates a DRS with a discourse referent X that has property P
question_word_sem(gi, lam('P', drs(['X'], [app('P', 'X')]))).

% ============================================
% LAMBDA CALCULUS OPERATIONS
% ============================================

% --- BETA REDUCTION ---
% Reduces lambda application: (\X.Body)(Arg) -> Body[X/Arg]
% Then recursively reduces the result
beta_reduce(app(lam(Var, Body), Arg), FinalResult) :-
    !,
    substitute(Var, Arg, Body, Result),
    beta_reduce(Result, FinalResult).  % Recursively reduce result!

% Reduce nested applications
beta_reduce(app(Func, Arg), Result) :-
    !,
    beta_reduce(Func, ReducedFunc),
    beta_reduce(Arg, ReducedArg),
    ( ReducedFunc = lam(_, _) ->
        % If reduced func is still a lambda, apply it
        beta_reduce(app(ReducedFunc, ReducedArg), Result)
    ;
        Result = app(ReducedFunc, ReducedArg)
    ).

% Reduce lambda bodies
beta_reduce(lam(Var, Body), lam(Var, ReducedBody)) :-
    !,
    beta_reduce(Body, ReducedBody).

% Base case: already reduced
beta_reduce(Term, Term).

% --- SUBSTITUTION ---
% substitute(Var, Value, Expression, Result)
% Replace all free occurrences of Var with Value in Expression
substitute(Var, Value, Var, Value) :- !.

substitute(Var, Value, lam(Var, Body), lam(Var, Body)) :- !.
% Variable is bound, don't substitute inside

substitute(Var, Value, lam(OtherVar, Body), lam(OtherVar, NewBody)) :-
    !,
    substitute(Var, Value, Body, NewBody).

substitute(Var, Value, app(Func, Arg), app(NewFunc, NewArg)) :-
    !,
    substitute(Var, Value, Func, NewFunc),
    substitute(Var, Value, Arg, NewArg).

substitute(Var, Value, drs(Universe, Conditions), drs(Universe, NewConditions)) :-
    !,
    maplist(substitute_in_condition(Var, Value), Conditions, NewConditions).

% Substitute in conj (conjunction)
substitute(Var, Value, conj(Conditions), conj(NewConditions)) :-
    !,
    maplist(substitute(Var, Value), Conditions, NewConditions).

% Substitute in compound terms (predicates with arguments)
substitute(Var, Value, Term, NewTerm) :-
    compound(Term),
    Term \= lam(_, _),
    Term \= app(_, _),
    Term \= drs(_, _),
    Term \= conj(_),
    !,
    Term =.. [Functor|Args],
    maplist(substitute(Var, Value), Args, NewArgs),
    NewTerm =.. [Functor|NewArgs].

substitute(_Var, _Value, Term, Term).
% Base case: atomic term or different variable

substitute_in_condition(Var, Value, Condition, NewCondition) :-
    substitute(Var, Value, Condition, NewCondition).

% --- APPLICATION HELPER ---
% apply(Function, Argument, Result)
% Applies a lambda function to an argument and reduces
apply(Func, Arg, Result) :-
    beta_reduce(app(Func, Arg), Result).

% ============================================
% DRS CONVERSION
% ============================================

% Convert fully reduced semantic representation to DRS
sem_to_drs(drs(U, C), drs(U, C)) :- !.
% Already a DRS

sem_to_drs(lam(_, _), _) :- !,
    % Still has unreduced lambda - should not happen
    fail.

sem_to_drs(Prop, drs([], [Prop])) :-
    % Simple proposition becomes DRS with no discourse referents
    \+ functor(Prop, drs, 2),
    \+ functor(Prop, lam, 2),
    \+ functor(Prop, app, 2).

% Handle conjunctions
sem_to_drs(conj(Conditions), drs([], Conditions)) :- !.

% ============================================
% DRS MERGING
% ============================================

% Merge two DRS structures
% drs_merge(DRS1, DRS2, MergedDRS)
drs_merge(drs(U1, C1), drs(U2, C2), drs(U, C)) :-
    append(U1, U2, U),
    append(C1, C2, C).

% ============================================
% DRS UTILITIES
% ============================================

% Resolve all app/2 terms in a DRS by applying beta reduction
resolve_drs(drs(U, Conditions), drs(NewU, FinalConditions)) :-
    maplist(resolve_condition, Conditions, ResolvedConditions),
    % Find all atom variables in both Universe and Conditions
    findall(V, (member(V, U), atom(V), atom_length(V, 1)), UniverseAtoms),
    term_variables(ResolvedConditions, PrologVarsInConds),
    find_atom_vars_in_term(ResolvedConditions, CondAtoms),
    append(UniverseAtoms, CondAtoms, AllAtomVars),
    sort(AllAtomVars, UniqueAtomVars),
    % Create mapping from atoms to fresh Prolog variables
    length(UniqueAtomVars, N),
    length(PrologVars, N),
    pairs_keys_values(Pairs, UniqueAtomVars, PrologVars),
    % Substitute all atom variables
    substitute_atom_vars(U, Pairs, NewU),
    substitute_atom_vars(ResolvedConditions, Pairs, FinalConditions).

resolve_condition(app(Func, Arg), Resolved) :-
    !,
    beta_reduce(app(Func, Arg), Resolved).

resolve_condition(conj(Conditions), conj(ResolvedConditions)) :-
    !,
    maplist(resolve_condition, Conditions, ResolvedConditions).

resolve_condition(Condition, Condition).

% Find all single-letter atoms (atom variables) in a term
find_atom_vars_in_term(Term, Atoms) :-
    find_atom_vars_in_term(Term, [], Atoms).

find_atom_vars_in_term(Atom, Acc, Atoms) :-
    atom(Atom),
    atom_length(Atom, 1),
    !,
    (member(Atom, Acc) -> Atoms = Acc ; Atoms = [Atom|Acc]).
find_atom_vars_in_term(Term, Acc, Atoms) :-
    compound(Term),
    !,
    Term =.. [_|Args],
    find_atom_vars_in_list(Args, Acc, Atoms).
find_atom_vars_in_term(_, Acc, Acc).

find_atom_vars_in_list([], Acc, Acc).
find_atom_vars_in_list([H|T], Acc, Atoms) :-
    find_atom_vars_in_term(H, Acc, Acc1),
    find_atom_vars_in_list(T, Acc1, Atoms).

% Substitute atom variables using a mapping
substitute_atom_vars(Term, Pairs, NewTerm) :-
    atom(Term),
    memberchk(Term-NewTerm, Pairs),
    !.
substitute_atom_vars(Term, Pairs, NewTerm) :-
    compound(Term),
    !,
    Term =.. [F|Args],
    maplist({Pairs}/[A,NA]>>substitute_atom_vars(A,Pairs,NA), Args, NewArgs),
    NewTerm =.. [F|NewArgs].
substitute_atom_vars(Term, _, Term).

% Old helper functions (keeping for reference)
% Convert atoms in Universe and Conditions to Prolog variables
% This allows Prolog's unification to work properly in prover
atoms_to_vars([], [], Conds, Conds).
atoms_to_vars([AtomVar|RestU], [PrologVar|RestPV], CondsIn, CondsOut) :-
    atom(AtomVar),
    !,
    % Replace all occurrences of AtomVar with PrologVar in conditions
    substitute_in_conditions(AtomVar, PrologVar, CondsIn, CondsTemp),
    atoms_to_vars(RestU, RestPV, CondsTemp, CondsOut).
atoms_to_vars([Var|RestU], [Var|RestPV], CondsIn, CondsOut) :-
    % Already a Prolog variable
    atoms_to_vars(RestU, RestPV, CondsIn, CondsOut).

% Substitute atom with Prolog variable in all conditions
substitute_in_conditions(_, _, [], []).
substitute_in_conditions(Atom, Var, [Cond|Rest], [NewCond|NewRest]) :-
    substitute_in_term(Atom, Var, Cond, NewCond),
    substitute_in_conditions(Atom, Var, Rest, NewRest).

% Substitute in a term
substitute_in_term(Atom, Var, Atom, Var) :- !.
substitute_in_term(Atom, Var, Term, NewTerm) :-
    compound(Term),
    !,
    Term =.. [Functor|Args],
    maplist(substitute_in_term(Atom, Var), Args, NewArgs),
    NewTerm =.. [Functor|NewArgs].
substitute_in_term(_, _, Term, Term).

% For backwards compatibility
to_drs(Sem, DRS) :- sem_to_drs(Sem, DRS).
