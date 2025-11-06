:- use_module(semantics/lexicon).

test :-
    NP = lam(p, app(p, huy)),
    VP = lam(x, conj([co(x, y), cho(y)])),
    format("NP: ~w~n", [NP]),
    format("VP: ~w~n", [VP]),
    
    % Create application term
    AppTerm = app(NP, VP),
    format("App term: ~w~n", [AppTerm]),
    
    % Beta reduce
    lexicon:beta_reduce(AppTerm, Result),
    format("Result: ~w~n", [Result]).
