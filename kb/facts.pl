:- module(facts, [fact/1, type/2, world_rule/1]).

% Thực thể & kiểu
type(huy, nguoi).
type(gau, cho).

% Sự kiện
fact(co(huy, gau)).
fact(cho(gau)).
fact(nho(gau)).
fact(ten(gau, gau)).
fact(mau_long(gau, nau)).
fact(hien(gau)).
fact(dat(huy, gau)).
fact(thich(huy, gau)).
fact(thich(gau, huy)).

% Luật suy diễn
world_rule(thich(X,Y)) :- fact(thich(Y,X)).
world_rule(cua(B, A)) :- fact(dat(A, B)). 
world_rule(cua(gau, huy)).