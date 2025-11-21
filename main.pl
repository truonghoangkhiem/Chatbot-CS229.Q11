:- initialization(main, main).
:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).
:- use_module(engine/fol).
:- use_module(engine/prover).

main :-
    prover:bootstrap,
    demo,
    halt.

demo :-
    run("Huy co cho khong", yn),
    run("Gau hien khong", yn),
    run("Gau nho khong", yn),
    run("Gau lon khong", yn),        % Thêm câu có đáp án No
    run("Ai dat Gau", who),
    run("Ai thich Gau", who),
    run("Huy co gi", what),
    run("Long cua Gau mau gi", what),
    run("con cho ten gi", what).

run(Sent, Kind) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        format("\n=== ~s ===~nTokens: ~w~n", [Sent, Toks]),
        format("Lambda Semantics: ~w~n", [Sem]),
        
        % Convert semantic representation to DRS
        sem_to_drs_wrapper(Kind, Sem, DRS),
        format("DRS: ~w~n", [DRS]),
        
        % Convert DRS to FOL
        fol:drs_to_fol(DRS, FOL),
        fol:fol_simplify(FOL, SimplifiedFOL),
        format("FOL: ~w~n", [SimplifiedFOL]),
        
        % Execute query
        exec(Kind, Sem, DRS)
    ;   format("\n=== ~s ===~n⛔ Không parse được~n", [Sent])
    ).

% Convert semantics to DRS based on question type
sem_to_drs_wrapper(yn, yn(Sem), ResolvedDRS) :-
    % First check if we need to beta-reduce
    (   Sem = lam(_, _) 
    ->  % Still has lambda, this shouldn't happen but handle it
        lexicon:beta_reduce(Sem, ReducedSem),
        lexicon:sem_to_drs(ReducedSem, DRS)
    ;   % Already reduced
        lexicon:sem_to_drs(Sem, DRS)
    ),
    % Resolve any app/2 terms and convert atoms to variables
    lexicon:resolve_drs(DRS, ResolvedDRS).

sem_to_drs_wrapper(who, who(Sem), ResolvedDRS) :-
    (Sem = drs(_, _) -> DRS = Sem ; lexicon:sem_to_drs(Sem, DRS)),
    % Resolve any app/2 terms inside DRS conditions
    lexicon:resolve_drs(DRS, ResolvedDRS).

sem_to_drs_wrapper(what, what(Sem), ResolvedDRS) :-
    % For what-questions, Sem is lam(Var, Body)
    % Convert to drs([Var], [Body])
    (   Sem = lam(Var, Body)
    ->  % Extract all atom variables from Body to include in Universe
        find_atom_vars(Body, BodyVars),
        % Filter out the question variable (Var)
        exclude(==(Var), BodyVars, OtherVars),
        % Create DRS with all variables in universe
        append([Var], OtherVars, AllVars),
        DRS = drs(AllVars, [Body])
    ;   Sem = drs(_, _) 
    ->  DRS = Sem 
    ;   lexicon:sem_to_drs(Sem, DRS)
    ),
    % Resolve any app/2 terms inside DRS conditions
    lexicon:resolve_drs(DRS, ResolvedDRS).

% Helper: find all single-letter atom variables in a term
find_atom_vars(Term, Vars) :-
    find_atom_vars_acc(Term, [], Vars).

find_atom_vars_acc(Atom, Acc, Vars) :-
    atom(Atom),
    atom_length(Atom, 1),
    !,
    (member(Atom, Acc) -> Vars = Acc ; Vars = [Atom|Acc]).
find_atom_vars_acc(conj(List), Acc, Vars) :-
    !,
    find_atom_vars_list(List, Acc, Vars).
find_atom_vars_acc(Term, Acc, Vars) :-
    compound(Term),
    !,
    Term =.. [_|Args],
    find_atom_vars_list(Args, Acc, Vars).
find_atom_vars_acc(_, Acc, Acc).

find_atom_vars_list([], Acc, Acc).
find_atom_vars_list([H|T], Acc, Vars) :-
    find_atom_vars_acc(H, Acc, Acc1),
    find_atom_vars_list(T, Acc1, Vars).

% Execute based on question type
exec(yn, yn(Sem), DRS) :-
    ( prover:prove_yn(DRS) -> 
        writeln("👉 Yes") 
    ; 
        writeln("👉 No") 
    ).

exec(who, who(_Sem), DRS) :-
    prover:answer_who(DRS, Ans), 
    format("👉 Who = ~w~n", [Ans]).

exec(what, what(_Sem), DRS) :-
    prover:answer_what(DRS, Ans), 
    format("👉 What = ~w~n", [Ans]).
