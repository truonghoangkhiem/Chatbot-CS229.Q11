:- module(grammar, [tokens/2, s/3]).
:- use_module('../semantics/lexicon').
:- set_prolog_flag(double_quotes, chars).

% --- Tokenizer tối giản
tokens(String, Tokens) :-
    split_string(String, " ", "?!.,", Words),
    exclude(=(""), Words, Ws),
    maplist(string_lower, Ws, Ls),
    maplist(atom_string, Tokens, Ls).

% ============================================
% SENTENCE STRUCTURE WITH SEMANTIC COMPOSITION
% ============================================

% --- YES/NO QUESTIONS ---
% S -> NP VP (opt 'khong')
% Semantics: Apply NP to VP using beta reduction
% NP: \P.P(entity), VP: \X.predicate(X)
% Result: (\P.P(entity))(\X.predicate(X)) -> predicate(entity)
s(yn(Sem)) --> np(NPSem), vp(VPSem), opt_khong, {
    lexicon:apply(NPSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.

% --- WH-QUESTIONS ---
% "Ai dat Gau?" -> Who dắt Gấu?
% Semantics: Question word takes VP as argument
s(who(Sem)) --> [ai], vp(VPSem), {
    lexicon:question_word_sem(ai, QSem),
    lexicon:apply(QSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.

% "Huy co gi?" -> Huy has what?
s(what(lam('X', FinalPred))) --> np(NPSem), v_tv(VerbSem), [gi], {
    % Extract entity from NP: Huy -> huy
    NPSem = lam(_, app(_, Entity)),
    % Extract predicate from verb: co -> \Y.\X.co(X,Y)
    VerbSem = lam(_, lam(_, Predicate)),
    % Substitute X (subject) with Entity: co(X,Y) -> co(huy,Y)
    lexicon:substitute('X', Entity, Predicate, Pred1),
    % Substitute Y (object) with 'X' (the question variable): co(huy,Y) -> co(huy,'X')
    lexicon:substitute('Y', 'X', Pred1, FinalPred)
}.

% "Long cua Gau mau gi?" -> Gấu's fur what color?
s(what(lam('X', mau_long(Entity, 'X')))) --> [long, cua], np(NPSem), [mau], [gi], {
    % Extract entity from NP
    NPSem = lam(_, app(_, Entity))
}.

% "Gau ten la gi?" -> Gấu's name is what?
s(what(lam('X', ten(Entity, 'X')))) --> np(NPSem), [ten, la], [gi], {
    % Extract entity from NP
    NPSem = lam(_, app(_, Entity))
}.

opt_khong --> [khong] ; [].

% ============================================
% NOUN PHRASES WITH LAMBDA SEMANTICS
% ============================================

% Proper nouns: \P.P(entity)
np(Sem) --> [Word], {
    member(Word, [huy, gau]),
    lexicon:noun_sem(Word, Sem)
}.

% ============================================
% VERB PHRASES WITH LAMBDA SEMANTICS
% ============================================

% Intransitive VP: \X.predicate(X)
vp(Sem) --> v_iv(Sem).

% Adjective as predicate: \X.property(X)
vp(Sem) --> adj(Sem).

% Transitive VP: Verb + Object (proper noun)
% Simple approach: extract entity from proper noun and substitute
vp(lam('X', SubstitutedPred)) --> v_tv(lam('Y', lam('X', Predicate))), [Word], {
    member(Word, [huy, gau]),
    lexicon:substitute('Y', Word, Predicate, SubstitutedPred)
}.

% Transitive VP with common noun: "co cho" (has dog)
% Simple direct semantics: \X.[co(X,Y) & cho(Y)] where Y is a new variable
% Use atoms for variable names instead of Prolog variables
vp(lam('X', conj([co('X', 'Y'), cho('Y')]))) --> [co], [cho].

% ============================================
% LEXICAL CATEGORIES
% ============================================

% --- Transitive Verbs ---
v_tv(Sem) --> [co], { lexicon:verb_tv_sem(co, Sem) }.
v_tv(Sem) --> [dat], { lexicon:verb_tv_sem(dat, Sem) }.
v_tv(Sem) --> [thich], { lexicon:verb_tv_sem(thich, Sem) }.

% --- Intransitive Verbs ---
v_iv(Sem) --> [hien], { lexicon:verb_iv_sem(hien, Sem) }.
v_iv(Sem) --> [nho], { lexicon:verb_iv_sem(nho, Sem) }.
v_iv(Sem) --> [lon], { lexicon:verb_iv_sem(lon, Sem) }.

% --- Adjectives ---
adj(Sem) --> [hien], { lexicon:adj_sem(hien, Sem) }.
adj(Sem) --> [nho], { lexicon:adj_sem(nho, Sem) }.

% --- Common Nouns ---
common_noun(Sem) --> [cho], { lexicon:noun_sem(cho, Sem) }.
common_noun(Sem) --> [nguoi], { lexicon:noun_sem(nguoi, Sem) }.
