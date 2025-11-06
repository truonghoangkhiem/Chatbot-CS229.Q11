% Quick test file
:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).

test :-
    Sent = "Huy co cho khong",
    grammar:tokens(Sent, Toks),
    format("Tokens: ~w~n", [Toks]),
    
    % Manual parse to see what's happening
    NPSem = lam(P, app(P, huy)),
    VPSem = lam(X, conj([co(X, _Y), cho(_Y)])),
    
    format("NP: ~w~n", [NPSem]),
    format("VP: ~w~n", [VPSem]),
    
    % Apply NP to VP
    lexicon:apply(NPSem, VPSem, Applied),
    format("After apply: ~w~n", [Applied]),
    
    % Beta reduce
    lexicon:beta_reduce(Applied, Reduced),
    format("After beta reduce: ~w~n", [Reduced]).
