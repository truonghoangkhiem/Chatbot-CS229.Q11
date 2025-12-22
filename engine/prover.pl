:- module(prover, [prove_yn/1, answer_who/2, answer_what/2, bootstrap/0]).
:- use_module('../kb/facts').

bootstrap :-
    forall(facts:fact(F), assertz(F)),
    forall(facts:world_rule(R), assertz(R)).

% --- PROVE YES/NO ---
prove_yn(drs(U, C)) :- 
    findall(U, prove_conds(C), L), 
    L \= [].

prove_conds([]).
prove_conds([C|Rest]) :- prove_one(C), prove_conds(Rest).

prove_one(conj(L)) :- !, prove_conds(L).
prove_one(type(X, T)) :- !, facts:type(X, T).
% Thử theo thứ tự: fact trực tiếp -> world_rule -> call động
prove_one(Pred) :- 
    (   facts:fact(Pred)           % 1. Thử fact trực tiếp
    ;   facts:world_rule(Pred)     % 2. Thử world_rule (suy diễn)
    ;   catch(call(Pred), _, fail) % 3. Fallback: call động (từ bootstrap)
    ). 

% --- ANSWER WHO ---
answer_who(drs([X|_], C), Ans) :-
    !,
    findall(X, (prove_conds(C), facts:type(X, nguoi)), L),
    sort(L, Ans).

answer_who(PropTemplate, Ans) :-
    arg(1, PropTemplate, X),
    findall(X, (call(PropTemplate), facts:type(X, nguoi)), L),
    sort(L, Ans).

% --- ANSWER WHAT ---
answer_what(drs([Y|_], C), Ans) :-
    !,
    findall(Y, prove_conds(C), L),
    sort(L, Ans).

answer_what(RelTemplate, Ans) :-
    arg(2, RelTemplate, Y),
    findall(Y, call(RelTemplate), L),
    sort(L, Ans).