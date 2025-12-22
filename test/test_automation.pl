:- module(test_automation, [run_tests/0]).
:- use_module('../syntax/grammar').
:- use_module('../semantics/lexicon').
:- use_module('../engine/prover').

% --- Test Cases ---
test_case(1, "Huy co cho khong",      yn, yes).
test_case(2, "Gau hien khong",        yn, yes).
test_case(3, "Gau nho khong",         yn, yes).
test_case(4, "Gau lon khong",         yn, no). 
test_case(5, "Huy dat Gau khong",     yn, yes).
test_case(6, "Ai dat Gau",            who, [huy]).
test_case(7, "Ai thich Gau",          who, [huy]).
test_case(8, "Huy co gi",             what, [gau]).
test_case(9, "con cho ten gi",        what, [gau]).
test_case(10, "Long cua Gau mau gi",  what, [nau]).

run_tests :-
    prover:bootstrap,
    findall(Id, test_case(Id, _, _, _), Ids),
    length(Ids, Count),
    format('~nDang chay ~w test cases...~n', [Count]),
    format('-----------------------------------------------------~n'),
    run_tests_loop(Ids, 0).

run_tests_loop([], Passed) :- 
    format('-----------------------------------------------------~n'),
    format('TONG KET: ~w PASSED.~n', [Passed]).

run_tests_loop([Id|Rest], Passed) :-
    test_case(Id, Sent, Kind, Expected),
    ( catch(execute_test(Sent, Kind, Actual), _, fail) ->
        ( Expected == Actual -> 
            Status = 'PASS', NewPassed is Passed + 1,
            format('[~w] ~w: "~s"~n', [Status, Id, Sent])
        ;   
            Status = 'FAIL', NewPassed is Passed,
            format('[~w] ~w: "~s" -> Mong doi: ~w, Thuc te: ~w~n', [Status, Id, Sent, Expected, Actual])
        )
    ;   
        Status = 'ERR ', NewPassed is Passed,
        format('[~w] ~w: "~s" -> Loi Parse/Logic~n', [Status, Id, Sent])
    ),
    run_tests_loop(Rest, NewPassed).

execute_test(Sent, Kind, Result) :-
    grammar:tokens(Sent, Toks),
    grammar:s(Sem, Toks, []),
    sem_to_drs_wrapper(Kind, Sem, DRS),
    get_result(Kind, DRS, Result).

% --- FIX LOGIC WRAPPER ---
sem_to_drs_wrapper(yn, yn(S), D) :- 
    (S=lam(_,_) -> lexicon:beta_reduce(S, R), lexicon:sem_to_drs(R, D0); lexicon:sem_to_drs(S, D0)), 
    lexicon:resolve_drs(D0, D).

sem_to_drs_wrapper(who, who(S), D) :- 
    (S=drs(_,_) -> D0=S; lexicon:sem_to_drs(S, D0)), 
    lexicon:resolve_drs(D0, D).

sem_to_drs_wrapper(what, what(S), D) :-
    ( S = lam(Var, drs(U, C)) ->    % Ưu tiên 1: Body là DRS
        D0 = drs([Var|U], C)
    ; S = lam(Var, Body) ->         % Ưu tiên 2: Body là predicate đơn
        term_variables(Body, _), 
        D0 = drs([Var], [Body])
    ; 
        D0 = S 
    ),
    lexicon:resolve_drs(D0, D).

get_result(yn, D, R) :- (prover:prove_yn(D) -> R = yes ; R = no).
get_result(who, D, R) :- prover:answer_who(D, R).
get_result(what, D, R) :- prover:answer_what(D, R).