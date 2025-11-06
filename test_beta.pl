:- use_module(semantics/lexicon).

test_direct :-
    Term1 = app(lam(x, f(x)), a),
    lexicon:beta_reduce(Term1, R1),
    format("Simple: ~w -> ~w~n", [Term1, R1]),
    
    Term2 = app(lam(p, app(p, huy)), lam(x, hien(x))),
    lexicon:beta_reduce(Term2, R2),
    format("Nested: ~w -> ~w~n", [Term2, R2]).
