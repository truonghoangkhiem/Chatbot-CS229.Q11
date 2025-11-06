% ==============================================================
% EXAMPLE QUERIES AND EXPECTED OUTPUTS
% Lambda Calculus-based Semantic Analysis System
% ==============================================================

% To run these examples:
% 1. Start SWI-Prolog: swipl
% 2. Load main: ?- [main].
% 3. Bootstrap: ?- prover:bootstrap.
% 4. Run individual tests below

% ==============================================================
% TEST SUITE
% ==============================================================

:- module(test_examples, [test_all/0, test_yn/0, test_who/0, test_what/0]).
:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).
:- use_module(engine/fol).
:- use_module(engine/prover).

% Initialize
:- initialization(prover:bootstrap).

% ==============================================================
% YES/NO QUESTIONS
% ==============================================================

test_yn :-
    writeln("\n========== YES/NO QUESTIONS =========="),
    test_yn_query("Gau hien khong", true),
    test_yn_query("Gau nho khong", true),
    test_yn_query("Huy dat Gau khong", true),
    test_yn_query("Huy thich Gau khong", true),
    test_yn_query("Gau thich Huy khong", true).

test_yn_query(Sent, Expected) :-
    format("\n--- Testing: ~s ---\n", [Sent]),
    grammar:tokens(Sent, Toks),
    format("Tokens: ~w\n", [Toks]),
    
    ( grammar:s(Sem, Toks, []) ->
        format("Parsed Semantics: ~w\n", [Sem]),
        
        % Show lambda term
        ( Sem = yn(LambdaTerm) ->
            format("Lambda Term: ~w\n", [LambdaTerm]),
            
            % Convert to DRS
            lexicon:sem_to_drs(LambdaTerm, DRS),
            format("DRS: ~w\n", [DRS]),
            
            % Convert to FOL
            fol:drs_to_fol(DRS, FOL),
            format("FOL: ~w\n", [FOL]),
            
            % Prove
            ( prover:prove_yn(DRS) ->
                format("Result: YES ✓\n", []),
                ( Expected = true -> 
                    writeln("✅ Test PASSED") 
                ; 
                    writeln("❌ Test FAILED (expected NO)") 
                )
            ;
                format("Result: NO ✗\n", []),
                ( Expected = false -> 
                    writeln("✅ Test PASSED") 
                ; 
                    writeln("❌ Test FAILED (expected YES)") 
                )
            )
        ;
            writeln("❌ Not a yes/no question")
        )
    ;
        writeln("⛔ Parse failed")
    ).

% ==============================================================
% WHO QUESTIONS
% ==============================================================

test_who :-
    writeln("\n========== WHO QUESTIONS =========="),
    test_who_query("Ai dat Gau", [huy]),
    test_who_query("Ai thich Gau", [huy]).

test_who_query(Sent, Expected) :-
    format("\n--- Testing: ~s ---\n", [Sent]),
    grammar:tokens(Sent, Toks),
    format("Tokens: ~w\n", [Toks]),
    
    ( grammar:s(Sem, Toks, []) ->
        format("Parsed Semantics: ~w\n", [Sem]),
        
        % Show structure
        ( Sem = who(InnerSem) ->
            format("Who-question semantics: ~w\n", [InnerSem]),
            
            % Should be a DRS
            ( InnerSem = drs(Universe, Conditions) ->
                format("DRS Universe: ~w\n", [Universe]),
                format("DRS Conditions: ~w\n", [Conditions]),
                
                % Convert to FOL
                fol:drs_to_fol(InnerSem, FOL),
                format("FOL: ~w\n", [FOL]),
                
                % Answer
                prover:answer_who(InnerSem, Answers),
                format("Answers: ~w\n", [Answers]),
                
                ( Answers = Expected ->
                    writeln("✅ Test PASSED")
                ;
                    format("❌ Test FAILED (expected ~w)\n", [Expected])
                )
            ;
                writeln("❌ Not a proper DRS")
            )
        ;
            writeln("❌ Not a who-question")
        )
    ;
        writeln("⛔ Parse failed")
    ).

% ==============================================================
% WHAT QUESTIONS
% ==============================================================

test_what :-
    writeln("\n========== WHAT QUESTIONS =========="),
    test_what_query("Long cua Gau mau gi", [nau]),
    test_what_query("Gau ten la gi", [gau]).

test_what_query(Sent, Expected) :-
    format("\n--- Testing: ~s ---\n", [Sent]),
    grammar:tokens(Sent, Toks),
    format("Tokens: ~w\n", [Toks]),
    
    ( grammar:s(Sem, Toks, []) ->
        format("Parsed Semantics: ~w\n", [Sem]),
        
        ( Sem = what(InnerSem) ->
            format("What-question semantics: ~w\n", [InnerSem]),
            
            ( InnerSem = drs(Universe, Conditions) ->
                format("DRS Universe: ~w\n", [Universe]),
                format("DRS Conditions: ~w\n", [Conditions]),
                
                fol:drs_to_fol(InnerSem, FOL),
                format("FOL: ~w\n", [FOL]),
                
                prover:answer_what(InnerSem, Answers),
                format("Answers: ~w\n", [Answers]),
                
                ( Answers = Expected ->
                    writeln("✅ Test PASSED")
                ;
                    format("❌ Test FAILED (expected ~w)\n", [Expected])
                )
            ;
                writeln("❌ Not a proper DRS")
            )
        ;
            writeln("❌ Not a what-question")
        )
    ;
        writeln("⛔ Parse failed")
    ).

% ==============================================================
% LAMBDA CALCULUS DEMONSTRATIONS
% ==============================================================

demo_lambda_reduction :-
    writeln("\n========== LAMBDA REDUCTION DEMO =========="),
    
    % Example 1: Simple application
    writeln("\n--- Example 1: (\\X.hien(X))(gau) ---"),
    Lambda1 = app(lam(X, hien(X)), gau),
    format("Input: ~w\n", [Lambda1]),
    lexicon:beta_reduce(Lambda1, Result1),
    format("Result: ~w\n", [Result1]),
    
    % Example 2: Curried application
    writeln("\n--- Example 2: (\\Y.\\X.dat(X,Y))(gau) ---"),
    Lambda2 = app(lam(Y, lam(X, dat(X, Y))), gau),
    format("Input: ~w\n", [Lambda2]),
    lexicon:beta_reduce(Lambda2, Result2),
    format("Result: ~w\n", [Result2]),
    
    % Example 3: Double application
    writeln("\n--- Example 3: ((\\Y.\\X.dat(X,Y))(gau))(huy) ---"),
    Lambda3 = app(app(lam(Y, lam(X, dat(X, Y))), gau), huy),
    format("Input: ~w\n", [Lambda3]),
    lexicon:beta_reduce(Lambda3, Result3),
    format("Result: ~w\n", [Result3]),
    
    % Example 4: NP applying to VP
    writeln("\n--- Example 4: (\\P.P(gau))(\\X.hien(X)) ---"),
    Lambda4 = app(lam(P, app(P, gau)), lam(X, hien(X))),
    format("Input: ~w\n", [Lambda4]),
    lexicon:beta_reduce(Lambda4, Result4),
    format("Result: ~w\n", [Result4]).

% ==============================================================
% DRS CONSTRUCTION DEMO
% ==============================================================

demo_drs_construction :-
    writeln("\n========== DRS CONSTRUCTION DEMO =========="),
    
    % Example 1: Simple proposition
    writeln("\n--- Example 1: Simple proposition ---"),
    lexicon:sem_to_drs(hien(gau), DRS1),
    format("Proposition: hien(gau)\n", []),
    format("DRS: ~w\n", [DRS1]),
    
    % Example 2: Question DRS
    writeln("\n--- Example 2: Who-question DRS ---"),
    QWord = lam(P, drs([X], [type(X, nguoi), app(P, X)])),
    VP = lam(X, dat(X, gau)),
    lexicon:apply(QWord, VP, Applied),
    lexicon:beta_reduce(Applied, DRS2),
    format("Question: Ai dat Gau?\n", []),
    format("DRS: ~w\n", [DRS2]),
    
    % Example 3: DRS to FOL
    writeln("\n--- Example 3: DRS to FOL conversion ---"),
    format("DRS: ~w\n", [DRS2]),
    fol:drs_to_fol(DRS2, FOL),
    format("FOL: ~w\n", [FOL]).

% ==============================================================
% RUN ALL TESTS
% ==============================================================

test_all :-
    writeln("\n╔════════════════════════════════════════════╗"),
    writeln("║  LAMBDA CALCULUS SEMANTIC PARSER TESTS    ║"),
    writeln("╚════════════════════════════════════════════╝"),
    demo_lambda_reduction,
    demo_drs_construction,
    test_yn,
    test_who,
    test_what,
    writeln("\n╔════════════════════════════════════════════╗"),
    writeln("║  ALL TESTS COMPLETED                       ║"),
    writeln("╚════════════════════════════════════════════╝\n").

% ==============================================================
% INTERACTIVE TESTING
% ==============================================================

% Usage in SWI-Prolog:
% ?- test_examples:test_all.
% ?- test_examples:test_yn.
% ?- test_examples:test_who.
% ?- test_examples:test_what.
% ?- test_examples:demo_lambda_reduction.
% ?- test_examples:demo_drs_construction.
