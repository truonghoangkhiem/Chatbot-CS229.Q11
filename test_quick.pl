% Quick Test File - CS229.Q11 QA System
% Chạy: swipl -g "consult('test_quick.pl'), run_tests, halt."

:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).
:- use_module(engine/fol).
:- use_module(engine/prover).

% ============================================
% MAIN TEST RUNNER
% ============================================
run_tests :-
    prover:bootstrap,
    writeln("================================================="),
    writeln("   QUICK TEST - HE THONG HOI DAP CS229.Q11"),
    writeln("================================================="),
    
    writeln("\n--- TEST 1: CAU HOI YES/NO ---"),
    test_yn("Huy co cho khong", yes),
    test_yn("Gau hien khong", yes),
    test_yn("Gau nho khong", yes),
    test_yn("Gau lon khong", no),
    
    writeln("\n--- TEST 2: CAU HOI WHO ---"),
    test_who("Ai dat Gau", [huy]),
    
    writeln("\n--- TEST 3: CAU HOI WHAT ---"),
    test_what("Huy co gi", [gau]),
    test_what("Long cua Gau mau gi", [nau]),
    
    writeln("\n--- TEST 4: TOKENIZER ---"),
    test_tokenizer,
    
    writeln("\n--- TEST 5: BETA REDUCTION ---"),
    test_beta_reduction,
    
    writeln("\n================================================="),
    writeln("   TEST COMPLETED!"),
    writeln("=================================================").

% ============================================
% YES/NO QUESTION TESTS
% ============================================
test_yn(Sent, Expected) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        sem_to_drs_wrapper(yn, Sem, DRS),
        (prover:prove_yn(DRS) -> Result = yes ; Result = no),
        ( Result == Expected ->
            format("  [PASS] ~s -> ~w~n", [Sent, Result])
        ;
            format("  [FAIL] ~s -> Got: ~w, Expected: ~w~n", [Sent, Result, Expected])
        )
    ;
        format("  [ERROR] Parse failed: ~s~n", [Sent])
    ).

% ============================================
% WHO QUESTION TESTS  
% ============================================
test_who(Sent, Expected) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        sem_to_drs_wrapper(who, Sem, DRS),
        prover:answer_who(DRS, Result),
        ( Result == Expected ->
            format("  [PASS] ~s -> ~w~n", [Sent, Result])
        ;
            format("  [FAIL] ~s -> Got: ~w, Expected: ~w~n", [Sent, Result, Expected])
        )
    ;
        format("  [ERROR] Parse failed: ~s~n", [Sent])
    ).

% ============================================
% WHAT QUESTION TESTS
% ============================================
test_what(Sent, Expected) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        sem_to_drs_wrapper(what, Sem, DRS),
        prover:answer_what(DRS, Result),
        ( Result == Expected ->
            format("  [PASS] ~s -> ~w~n", [Sent, Result])
        ;
            format("  [FAIL] ~s -> Got: ~w, Expected: ~w~n", [Sent, Result, Expected])
        )
    ;
        format("  [ERROR] Parse failed: ~s~n", [Sent])
    ).

% ============================================
% TOKENIZER TEST
% ============================================
test_tokenizer :-
    Test1 = "Huy co cho khong",
    grammar:tokens(Test1, Toks1),
    ( Toks1 == [huy, co, cho, khong] ->
        format("  [PASS] Tokenize '~s' -> ~w~n", [Test1, Toks1])
    ;
        format("  [FAIL] Tokenize '~s' -> ~w~n", [Test1, Toks1])
    ),
    
    Test2 = "Long cua Gau mau gi",
    grammar:tokens(Test2, Toks2),
    ( Toks2 == [long, cua, gau, mau, gi] ->
        format("  [PASS] Tokenize '~s' -> ~w~n", [Test2, Toks2])
    ;
        format("  [FAIL] Tokenize '~s' -> ~w~n", [Test2, Toks2])
    ).

% ============================================
% BETA REDUCTION TEST
% ============================================
test_beta_reduction :-
    % Test: (\P.P(huy))(\X.hien(X)) => hien(huy)
    Term = app(lam('P', app('P', huy)), lam('X', hien('X'))),
    lexicon:beta_reduce(Term, Result),
    ( Result == hien(huy) ->
        format("  [PASS] Beta reduce: ~w -> ~w~n", [Term, Result])
    ;
        format("  [FAIL] Beta reduce: ~w -> ~w (expected hien(huy))~n", [Term, Result])
    ).

% ============================================
% HELPER WRAPPERS (same as main.pl)
% ============================================
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

% ============================================
% MANUAL DEBUG TEST (for interactive use)
% ============================================
test :-
    Sent = "Huy co cho khong",
    grammar:tokens(Sent, Toks),
    format("Tokens: ~w~n", [Toks]),
    
    ( grammar:s(Sem, Toks, []) ->
        format("Semantics: ~w~n", [Sem]),
        sem_to_drs_wrapper(yn, Sem, DRS),
        format("DRS: ~w~n", [DRS]),
        fol:drs_to_fol(DRS, FOL),
        format("FOL: ~w~n", [FOL])
    ;
        writeln("Parse failed!")
    ).
