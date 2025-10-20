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
    run("Ai dat Gau", who),
    run("Ai thich Gau", who),
    run("Huy co gi", what),
    run("Long cua Gau mau gi", what),
    run("Gau ten la gi", what).

run(Sent, Kind) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        lexicon:to_drs(Sem, DRS),
        fol:drs_to_fol(DRS, FOL),
        format("\n=== ~s ===~nTokens: ~w~nDRS: ~w~nFOL: ~w~n", [Sent, Toks, DRS, FOL]),
        exec(Kind, Sem)
    ;   format("\n=== ~s ===~n⛔ Không parse được~n", [Sent])
    ).

exec(yn, yn(Prop)) :-
    ( prover:prove_yn(Prop) -> writeln("👉 Yes") ; writeln("👉 No") ).
exec(who, who(_X,Prop)) :-
    prover:answer_who(Prop, Ans), format("👉 Who = ~w~n", [Ans]).
exec(what, what(_X,Rel)) :-
    prover:answer_what(Rel, Ans), format("👉 What = ~w~n", [Ans]).
