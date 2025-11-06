:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).

test :-
    Sent = "Huy co cho khong",
    grammar:tokens(Sent, Toks),
    format("Tokens: ~w~n", [Toks]),
    ( grammar:s(Sem, Toks, []) ->
        format("Parsed: ~w~n", [Sem])
    ; writeln("Parse failed") ).
