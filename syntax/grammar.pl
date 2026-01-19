:- module(grammar, [tokens/2, s/3]).
:- use_module('../semantics/lexicon').
:- set_prolog_flag(double_quotes, chars).

% --- Tokenizer ---
tokens(String, Tokens) :-
    split_string(String, " ", "?!.,", Words),
    exclude(=(""), Words, Ws),
    maplist(string_lower, Ws, Ls),
    maplist(atom_string, Tokens, Ls).

% ============================================
% 1. SENTENCE STRUCTURE
% ============================================

% --- YES/NO QUESTIONS ---
s(yn(Sem)) --> np(NPSem), vp(VPSem), opt_khong, {
    lexicon:apply(NPSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.

% --- WHO QUESTIONS ---
s(who(Sem)) --> [ai], vp(VPSem), {
    lexicon:question_word_sem(ai, QSem),
    lexicon:apply(QSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.

s(who(Sem)) --> [ai], v_tv(VerbSem), np(ObjSem), {
    ( ObjSem = lam(_, app(_, Entity)) -> TrueObj = Entity ; TrueObj = 'Unknown' ),
    lexicon:apply(VerbSem, TrueObj, VP),
    lexicon:question_word_sem(ai, QSem),
    lexicon:apply(QSem, VP, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.

% --- WHAT QUESTIONS ---
s(what(lam('X', FinalPred))) --> np(NPSem), v_tv(VerbSem), [gi], {
    NPSem = lam(_, app(_, Entity)),         
    VerbSem = lam(_, lam(_, Predicate)),    
    lexicon:substitute('X', Entity, Predicate, P1), 
    lexicon:substitute('Y', 'X', P1, FinalPred)     
}.

s(what(Sem)) --> np(NPSem), [ten], opt_la, [gi], {
    VerbLambda = lam('E', ten('E', 'X')),
    lexicon:apply(NPSem, VerbLambda, BodyDRS),
    Sem = lam('X', BodyDRS)
}.

s(what(lam('Y', mau_long(Entity, 'Y')))) --> 
    [long, cua], np(NPSem), [mau, gi], {
    NPSem = lam(_, app(_, Entity))
}.

% [NEW] Cấu trúc câu trần thuật (Declarative) dùng cho ngữ cảnh
% Ví dụ: "Huy có một con chó"
s(decl(Sem)) --> np(NPSem), vp(VPSem), {
    lexicon:apply(NPSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.

opt_khong --> [khong] ; [].
opt_la --> [la] ; [].

% ============================================
% 2. NOUN PHRASES
% ============================================

np(Sem) --> np_base(BaseSem), np_extend(BaseSem, Sem).

% --- Base ---
np_base(Sem) --> [Word], { member(Word, [huy, gau]), lexicon:noun_sem(Word, Sem) }.
np_base(Sem) --> [no], { lexicon:noun_sem(no, Sem) }. % [NEW] Đại từ 'nó'
np_base(Sem) --> common_noun(Sem).

% [NEW] "một con chó" -> tạo DRS tồn tại: \P.drs([X], [cho(X), P(X)])
np_base(lam('P', drs(['X'], [app(NounSem, 'X'), app('P', 'X')]))) --> 
    [mot], [con], common_noun(NounSem).
np_base(lam('P', drs(['Y'], [cho('Y'), app('P', 'Y')]))) --> [con, cho].
np_base(lam('X', long('X'))) --> [long].

% --- Extension ---
np_extend(S, S) --> [].
np_extend(HeadSem, FinalSem) --> 
    [cua], np(ModifierSem), 
    {
        CombinedSem = lam('X', conj([
            app(HeadSem, 'X'), 
            app(ModifierSem, lam('M', cua('X', 'M')))
        ]))
    },
    np_extend(CombinedSem, FinalSem).

% ============================================
% 3. VERB PHRASES
% ============================================

vp(Sem) --> v_iv(Sem).
vp(Sem) --> adj(Sem).

% Rule đặc biệt cho "có một con chó" - phải đặt TRƯỚC rule tổng quát
vp(Sem) --> v_tv(VerbSem), [mot, con, cho], {
    % Verb: \Y.\X.co(X,Y)
    % VP: \X.drs([Z], [cho(Z), co(X,Z)])
    VerbSem = lam(YVar, lam(SubjVar, Pred)),
    lexicon:substitute(YVar, 'Z', Pred, P1),
    Sem = lam(SubjVar, drs(['Z'], [cho('Z'), P1]))
}.

vp(Sem) --> v_tv(VerbSem), np(ObjSem), {
    ( ObjSem = lam(_, app(_, Entity)), atom(Entity), Entity \= 'REF' ->
        % Case 1: Object là thực thể cụ thể (Gau, Huy)
        lexicon:apply(VerbSem, Entity, ReducedSem),
        lexicon:beta_reduce(ReducedSem, Sem)
    ; ObjSem = lam('P', drs([ObjVar], Conditions)) ->
        % Case 2: Object là lượng từ (một con chó)
        term_variables(Conditions, _),
        lexicon:substitute('P', lam(ObjVar, 'PLACEHOLDER'), ObjSem, _),
        
        VerbSem = lam('Y', lam(SubjVar, Rel)),
        lexicon:substitute('Y', ObjVar, Rel, RelWithObj),
        Sem = lam(SubjVar, drs([ObjVar], [Conditions, RelWithObj]))
    ; ObjSem = lam(_, app(_, 'REF')) ->
        % Case 3: Object là đại từ (nó)
        lexicon:apply(VerbSem, 'REF', ReducedSem),
        lexicon:beta_reduce(ReducedSem, Sem)
    ; ObjSem = lam(ObjVar, TypePred), \+ TypePred = app(_,_) ->
        % Case 4: Object là common noun (cho, nguoi) - "Huy co cho"
        % VerbSem = lam('Y', lam('X', co('X','Y')))
        % Tạo DRS tồn tại: \X.drs([ObjY], [cho(ObjY), co(X, ObjY)])
        VerbSem = lam(YVar, lam(SubjVar, Rel)),
        lexicon:substitute(ObjVar, 'ObjY', TypePred, NewTypePred),
        lexicon:substitute(YVar, 'ObjY', Rel, RelWithY),
        Sem = lam(SubjVar, drs(['ObjY'], [NewTypePred, RelWithY]))
    ;
        Sem = lam('X', fail)
    )
}.

v_tv(Sem) --> [Word], { member(Word, [co, dat, thich]), lexicon:verb_tv_sem(Word, Sem) }.
v_iv(Sem) --> [Word], { member(Word, [hien, nho, lon]), lexicon:verb_iv_sem(Word, Sem) }.
adj(Sem)  --> [Word], { member(Word, [hien, nho, lon, nau]), lexicon:adj_sem(Word, Sem) }.
common_noun(Sem) --> [Word], { member(Word, [cho, nguoi]), lexicon:noun_sem(Word, Sem) }.