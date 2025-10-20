:- module(prover, [prove_yn/1, answer_who/2, answer_what/2, bootstrap/0]).
:- use_module(kb/facts).

bootstrap :-
    forall(facts:fact(F), assertz(F)),
    forall(facts:world_rule(R), assertz(R)).

% Yes/No
prove_yn(conj(Cs)) :- maplist(call, Cs), !.
prove_yn(Prop)     :- call(Prop), !.
prove_yn(_)        :- fail.

% who: thực thể ở đối số 1 và là người
answer_who(PropTemplate, Answers) :-
    arg(1, PropTemplate, X),
    findall(X, (call(PropTemplate), facts:type(X, nguoi)), Bag),
    sort(Bag, Answers).

% what: đối số 2 làm đáp án
answer_what(RelTemplate, Answers) :-
    arg(2, RelTemplate, Y),
    findall(Y, call(RelTemplate), Bag0),
    sort(Bag0, Answers).
