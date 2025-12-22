:- module(lexicon, [
    noun_sem/2, verb_iv_sem/2, verb_tv_sem/2, adj_sem/2,
    question_word_sem/2,
    apply/3, beta_reduce/2, substitute/4,
    sem_to_drs/2, resolve_drs/2
]).

% --- TỪ VỰNG LAMBDA ---
noun_sem(huy, lam('P', app('P', huy))).
noun_sem(gau, lam('P', app('P', gau))).
noun_sem(cho, lam('X', cho('X'))).
noun_sem(nguoi, lam('X', nguoi('X'))).

verb_iv_sem(hien, lam('X', hien('X'))).
verb_iv_sem(nho, lam('X', nho('X'))).
verb_iv_sem(lon, lam('X', lon('X'))).

verb_tv_sem(co, lam('Y', lam('X', co('X', 'Y')))).
verb_tv_sem(dat, lam('Y', lam('X', dat('X', 'Y')))).
verb_tv_sem(thich, lam('Y', lam('X', thich('X', 'Y')))).

adj_sem(hien, lam('X', hien('X'))).
adj_sem(nho, lam('X', nho('X'))).
adj_sem(lon, lam('X', lon('X'))).

question_word_sem(ai, lam('P', drs(['X'], [type('X', nguoi), app('P', 'X')]))).
question_word_sem(gi, lam('P', drs(['X'], [app('P', 'X')]))).

% --- BETA REDUCTION ---
beta_reduce(app(lam(Var, Body), Arg), FinalResult) :- !,
    substitute(Var, Arg, Body, Result),
    beta_reduce(Result, FinalResult).
beta_reduce(app(Func, Arg), Result) :- !,
    beta_reduce(Func, RF), beta_reduce(Arg, RA),
    (RF = lam(_,_) -> beta_reduce(app(RF, RA), Result) ; Result = app(RF, RA)).
beta_reduce(lam(V, B), lam(V, RB)) :- !, beta_reduce(B, RB).
beta_reduce(conj(List), conj(RList)) :- !, maplist(beta_reduce, List, RList).
beta_reduce(Term, Term).

% --- SUBSTITUTION ---
substitute(Var, Val, Var, Val) :- !.
substitute(Var, _Val, lam(Var, Body), lam(Var, Body)) :- !.
substitute(Var, Val, lam(OV, Body), lam(OV, NB)) :- !, substitute(Var, Val, Body, NB).
substitute(Var, Val, app(F, A), app(NF, NA)) :- !, substitute(Var, Val, F, NF), substitute(Var, Val, A, NA).
substitute(Var, Val, drs(U, C), drs(U, NC)) :- !, maplist(substitute_in_cond(Var, Val), C, NC).
substitute(Var, Val, conj(L), conj(NL)) :- !, maplist(substitute(Var, Val), L, NL).
substitute(Var, Val, Term, NTerm) :- 
    compound(Term), Term =.. [F|Args], 
    maplist(substitute(Var, Val), Args, NArgs), NTerm =.. [F|NArgs], !.
substitute(_, _, T, T).

substitute_in_cond(Var, Val, Cond, NCond) :- substitute(Var, Val, Cond, NCond).
apply(F, A, R) :- beta_reduce(app(F, A), R).

% --- DRS UTILS ---
sem_to_drs(drs(U, C), drs(U, C)) :- !.
sem_to_drs(Prop, drs([], [Prop])).

% Helper: Chuyển đổi Atom Variable ('X') thành Prolog Variable (_G...)
resolve_drs(drs(U, C), drs(NewU, FinalC)) :-
    maplist(resolve_cond, C, ReducedC),
    ( U == [] -> 
        NewU = [], FinalC = ReducedC
    ;
        maplist(create_var_mapping, U, Pairs),
        substitute_vars_in_list(ReducedC, Pairs, FinalC),
        pairs_values(Pairs, NewU)
    ).

resolve_cond(app(F, A), R) :- !, beta_reduce(app(F, A), R).
resolve_cond(conj(L), conj(RL)) :- !, maplist(resolve_cond, L, RL).
resolve_cond(C, C).

create_var_mapping(Atom, Atom-Var) :- var(Var). 
pairs_values([], []).
pairs_values([_-V|T], [V|VT]) :- pairs_values(T, VT).

substitute_vars_in_list([], _, []).
substitute_vars_in_list([H|T], P, [NH|NT]) :- substitute_vars(H, P, NH), substitute_vars_in_list(T, P, NT).

substitute_vars(Term, Pairs, NewTerm) :-
    atom(Term), member(Term-Var, Pairs), !, NewTerm = Var.
substitute_vars(Term, Pairs, NewTerm) :-
    compound(Term), !, Term =.. [F|Args],
    maplist(substitute_vars_helper(Pairs), Args, NArgs), NewTerm =.. [F|NArgs].
substitute_vars(T, _, T).
substitute_vars_helper(P, A, NA) :- substitute_vars(A, P, NA).