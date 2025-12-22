:- module(grammar, [tokens/2, s/3]).
:- use_module('../semantics/lexicon').
:- set_prolog_flag(double_quotes, chars).

% --- Tokenizer
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

% "Huy co gi?"
s(what(lam('X', FinalPred))) --> np(NPSem), v_tv(VerbSem), [gi], {
    NPSem = lam(_, app(_, Entity)),         
    VerbSem = lam(_, lam(_, Predicate)),    
    lexicon:substitute('X', Entity, Predicate, P1), 
    lexicon:substitute('Y', 'X', P1, FinalPred)     
}.

% "Con cho ten gi?" -> FIX LỖI TẠI ĐÂY (Dùng biến 'X' thay vì 'Name')
s(what(Sem)) --> np(NPSem), [ten], opt_la, [gi], {
    % Tạo hàm lambda: \E.ten(E, X)
    VerbLambda = lam('E', ten('E', 'X')),
    
    % Áp dụng NP vào hàm này
    lexicon:apply(NPSem, VerbLambda, BodyDRS),
    
    % Kết quả: \X.BodyDRS
    Sem = lam('X', BodyDRS)
}.

% "Long cua Gau mau gi?"
s(what(lam('Y', mau_long(Entity, 'Y')))) --> 
    [long, cua], np(NPSem), [mau, gi], {
    NPSem = lam(_, app(_, Entity))
}.

opt_khong --> [khong] ; [].
opt_la --> [la] ; [].

% ============================================
% 2. NOUN PHRASES (KHỬ ĐỆ QUY TRÁI)
% ============================================

np(Sem) --> np_base(BaseSem), np_extend(BaseSem, Sem).

% --- Base ---
np_base(Sem) --> [Word], { member(Word, [huy, gau]), lexicon:noun_sem(Word, Sem) }.
np_base(Sem) --> common_noun(Sem).
np_base(lam('P', drs(['Y'], [cho('Y'), app('P', 'Y')]))) --> [con, cho].
np_base(lam('X', long('X'))) --> [long].

% --- Extension (Đệ quy phải) ---
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

vp(Sem) --> v_tv(VerbSem), np(ObjSem), {
    ( ObjSem = lam(_, app(_, Entity)) ->
        lexicon:apply(VerbSem, Entity, ReducedSem),
        lexicon:beta_reduce(ReducedSem, Sem)
    ; ObjSem = lam(Var, Prop) ->
        VerbSem = lam('Y', lam('X', AtomicPred)), 
        ObjVar = 'Obj', SubjVar = 'Subj',
        lexicon:substitute('Y', ObjVar, AtomicPred, P1),
        lexicon:substitute('X', SubjVar, P1, FinalPred),
        lexicon:substitute(Var, ObjVar, Prop, SubstProp),
        Sem = lam(SubjVar, drs([ObjVar], [SubstProp, FinalPred]))
    ;
        Sem = lam('X', fail)
    )
}.

v_tv(Sem) --> [Word], { member(Word, [co, dat, thich]), lexicon:verb_tv_sem(Word, Sem) }.
v_iv(Sem) --> [Word], { member(Word, [hien, nho, lon]), lexicon:verb_iv_sem(Word, Sem) }.
adj(Sem)  --> [Word], { member(Word, [hien, nho, lon]), lexicon:adj_sem(Word, Sem) }.
common_noun(Sem) --> [Word], { member(Word, [cho, nguoi]), lexicon:noun_sem(Word, Sem) }.