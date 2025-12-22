:- initialization(main, main).
:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).
:- use_module(engine/fol).
:- use_module(engine/prover).

main :-
    prover:bootstrap,
    writeln("--- DEMO START ---"),
    run("Huy co cho khong", yn),
    run("Gau hien khong", yn),
    run("Ai dat Gau", who),
    run("Huy co gi", what),
    run("con cho ten gi", what),
    run("Long cua Gau mau gi", what),
    halt.

run(Sent, Kind) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        format("~nQ: ~s~n", [Sent]),
        
        % Chuyển đổi Semantics -> DRS
        sem_to_drs_wrapper(Kind, Sem, DRS),
        
        fol:drs_to_fol(DRS, FOL),
        format("FOL: ~w~n", [FOL]),
        
        % Thực thi
        (Kind=yn -> (prover:prove_yn(DRS) -> A='Yes'; A='No');
         Kind=who -> prover:answer_who(DRS, A);
         prover:answer_what(DRS, A)),
        format("Ans: ~w~n", [A])
    ;   format("Parse Failed: ~s~n", [Sent])
    ).

% Helper wrappers (giống test_automation.pl)
sem_to_drs_wrapper(yn, yn(S), D) :- 
    (S=lam(_,_) -> lexicon:beta_reduce(S, R), lexicon:sem_to_drs(R, D0); lexicon:sem_to_drs(S, D0)), 
    lexicon:resolve_drs(D0, D).

sem_to_drs_wrapper(who, who(S), D) :- 
    (S=drs(_,_) -> D0=S; lexicon:sem_to_drs(S, D0)), 
    lexicon:resolve_drs(D0, D).

sem_to_drs_wrapper(what, what(S), D) :-
    ( S = lam(Var, drs(U, C)) -> D0 = drs([Var|U], C)
    ; S = lam(Var, Body) -> term_variables(Body, _), D0 = drs([Var], [Body])
    ; D0 = S ),
    lexicon:resolve_drs(D0, D).