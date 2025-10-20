:- module(grammar, [tokens/2, s/3]).
:- set_prolog_flag(double_quotes, chars).

% --- Tokenizer tối giản
tokens(String, Tokens) :-
    split_string(String, " ", "?!.,", Words),
    exclude(=(""), Words, Ws),
    maplist(string_lower, Ws, Ls),
    maplist(atom_string, Tokens, Ls).

% ===== CẤU TRÚC CÂU =====
% Yes/No: cho phép đuôi 'khong'
s(yn(Prop))        --> np(S), vp_yn(S, Prop), opt_khong.

% Who / What (giữ như cũ)
s(who(X,Prop))     --> [ai], vp_who(X, Prop).
s(what(X,Rel))     --> what_q(X, Rel).

opt_khong --> [khong] ; [].

% ===== CỤM DANH TỪ =====
np(huy) --> [huy].
np(gau) --> [gau].

% ===== VỊ NGỮ DÙNG CHO YN =====
% - Theo TÊN: "Huy co Gau khong" -> co(huy,gau)
vp_yn(S, co(S,O))            --> v_co, obj_name(O).
vp_yn(S, dat(S,O))           --> v_dat, obj_name(O).
vp_yn(S, thich(S,O))         --> v_thich, obj_name(O).
vp_yn(S, hien(S))            --> adj_hien.
vp_yn(S, nho(S))             --> adj_nho.

% - Theo LOẠI: "Huy co cho khong" -> conj([co(S,O), cho(O)])
vp_yn(S, conj([co(S,O), cho(O)])) --> v_co, obj_kind(cho,O).

% ===== VỊ NGỮ DÙNG CHO WHO =====
vp_who(X, dat(X,O))          --> v_dat, obj_name(O).
vp_who(X, thich(X,O))        --> v_thich, obj_name(O).

% ===== ĐỐI TƯỢNG =====
obj_name(huy) --> [huy].
obj_name(gau) --> [gau].
% theo LOẠI (O là biến, sẽ được ràng buộc bởi predicate loại)
obj_kind(cho, _O) --> [cho].

% ===== TỪ VỰNG =====
v_co     --> [co].
v_dat    --> [dat].
v_thich  --> [thich].
adj_hien --> [hien].
adj_nho  --> [nho].

% ===== WHAT-Questions =====
what_q(X, co(X, O))          --> np(X), [co, gi], {O = _}.
what_q(X, mau_long(X, M))    --> [long, cua], np(X), [mau, gi], {M = _}.
what_q(X, ten(X, T))         --> np(X), [ten, la, gi], {T = _}.
