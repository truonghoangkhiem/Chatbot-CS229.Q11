# Technical Implementation Guide

## Module-by-Module Deep Dive

---

## 1. semantics/lexicon.pl - Lambda Expressions & Beta Reduction

### 1.1 Representation of Lambda Terms

Lambda terms are represented using Prolog functors:

```prolog
% Lambda abstraction: \X.Body
lam(Variable, Body)

% Function application: F(A)
app(Function, Argument)

% DRS structure
drs(Universe, Conditions)
```

### 1.2 Examples of Lexical Entries

#### Proper Noun: "Gau"
```prolog
noun_sem(gau, lam(P, app(P, gau))).
```
This means: λP.P(gau)

**Type**: (e → t) → t
- Takes a predicate P (type e → t)
- Applies P to the entity gau
- Returns a truth value t

#### Intransitive Verb: "hien" (gentle)
```prolog
verb_iv_sem(hien, lam(X, hien(X))).
```
This means: λX.hien(X)

**Type**: e → t
- Takes an entity X
- Returns whether X is hien (gentle)

#### Transitive Verb: "dat" (walk/lead)
```prolog
verb_tv_sem(dat, lam(Y, lam(X, dat(X, Y)))).
```
This means: λY.λX.dat(X, Y)

**Type**: e → (e → t)
- First takes object Y (curried)
- Then takes subject X
- Returns whether X walks/leads Y

### 1.3 Beta Reduction Algorithm

The `beta_reduce/2` predicate implements the core reduction:

```prolog
beta_reduce(app(lam(Var, Body), Arg), Result) :-
    substitute(Var, Arg, Body, Result).
```

**Example Execution**:
```prolog
?- beta_reduce(app(lam(X, hien(X)), gau), R).
R = hien(gau).
```

**Step-by-step**:
1. Match: `app(lam(X, hien(X)), gau)`
2. Call: `substitute(X, gau, hien(X), R)`
3. Result: `R = hien(gau)`

### 1.4 Substitution Algorithm

```prolog
substitute(Var, Value, Expression, Result)
```

**Cases**:

1. **Variable match**: `substitute(X, gau, X, gau)` ✓
2. **Bound variable** (capture avoidance):
   ```prolog
   substitute(X, value, lam(X, Body), lam(X, Body))
   ```
   X is bound in the lambda, don't substitute inside.

3. **Free variable in lambda**:
   ```prolog
   substitute(X, value, lam(Y, X), lam(Y, value))
   ```

4. **Application**:
   ```prolog
   substitute(X, V, app(F, A), app(F', A')) :-
       substitute(X, V, F, F'),
       substitute(X, V, A, A').
   ```

### 1.5 DRS Utilities

```prolog
% Merge two DRS structures
drs_merge(drs(U1, C1), drs(U2, C2), drs(U, C)) :-
    append(U1, U2, U),
    append(C1, C2, C).

% Resolve embedded applications in DRS
resolve_drs(drs(U, Conditions), drs(U, ResolvedConditions)) :-
    maplist(resolve_condition, Conditions, ResolvedConditions).
```

---

## 2. syntax/grammar.pl - Semantic Composition

### 2.1 Core Principle

Each grammar rule performs **both** syntactic and semantic composition:

```prolog
s(Semantics) --> constituent1(Sem1), constituent2(Sem2), {
    % Semantic computation
    compose(Sem1, Sem2, Semantics)
}.
```

### 2.2 S → NP VP (Yes/No Questions)

```prolog
s(yn(Sem)) --> np(NPSem), vp(VPSem), opt_khong, {
    lexicon:apply(NPSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

**Example**: "Gau hien"

1. **Parse NP**: `np(lam(P, app(P, gau)))`
2. **Parse VP**: `vp(lam(X, hien(X)))`
3. **Apply**:
   ```prolog
   apply(lam(P, app(P, gau)), lam(X, hien(X)), Result)
   = app(lam(P, app(P, gau)), lam(X, hien(X)))
   ```
4. **Beta reduce**:
   ```prolog
   beta_reduce(app(lam(P, app(P, gau)), lam(X, hien(X))), Sem)
   = beta_reduce(app(lam(X, hien(X)), gau), Sem)
   = hien(gau)
   ```

### 2.3 VP → V NP (Transitive)

```prolog
vp(Sem) --> v_tv(VerbSem), np(ObjSem), {
    lexicon:apply(VerbSem, ObjSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

**Example**: "dat Gau" (walk Gau)

1. **Verb**: `lam(Y, lam(X, dat(X, Y)))`
2. **Object**: `lam(P, app(P, gau))`
3. **Apply verb to object**:
   ```
   app(lam(Y, lam(X, dat(X, Y))), lam(P, app(P, gau)))
   ```
4. **Beta reduce**:
   ```
   substitute(Y, lam(P, app(P, gau)), lam(X, dat(X, Y)), R)
   = lam(X, dat(X, lam(P, app(P, gau))))
   ```
   
   But wait! We need to apply `lam(P, app(P, gau))` as if it were an entity.
   
   **Solution**: The object NP type-raises and applies to the predicate position:
   ```
   app(lam(P, app(P, gau)), lam(Y, lam(X, dat(X, Y))))
   = app(lam(Y, lam(X, dat(X, Y))), gau)
   = lam(X, dat(X, gau))
   ```

### 2.4 Who-Questions

```prolog
s(who(Sem)) --> [ai], vp(VPSem), {
    lexicon:question_word_sem(ai, QSem),
    lexicon:apply(QSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

**Example**: "Ai dat Gau?"

1. **Q-word**: `lam(P, drs([X], [type(X, nguoi), app(P, X)]))`
2. **VP**: `lam(X, dat(X, gau))`
3. **Apply**:
   ```
   app(lam(P, drs([X], [type(X, nguoi), app(P, X)])), 
       lam(X, dat(X, gau)))
   ```
4. **Beta reduce**:
   ```
   substitute(P, lam(X, dat(X, gau)), 
              drs([X], [type(X, nguoi), app(P, X)]), R)
   = drs([X], [type(X, nguoi), app(lam(X, dat(X, gau)), X)])
   ```
5. **Further reduce the embedded app**:
   ```
   = drs([X], [type(X, nguoi), dat(X, gau)])
   ```

---

## 3. engine/fol.pl - DRS to First-Order Logic

### 3.1 Conversion Rules

```prolog
drs_to_fol(drs([], Conditions), and(Conditions)).
% No existential quantifiers

drs_to_fol(drs([Var|Vars], Conditions), exists(Var, RestFOL)) :-
    drs_to_fol(drs(Vars, Conditions), RestFOL).
% Nest existentials
```

### 3.2 Example Conversions

#### Simple DRS
```prolog
drs([], [hien(gau)])
→ hien(gau)
```

#### Existential DRS
```prolog
drs([X], [type(X, nguoi), dat(X, gau)])
→ exists(X, and(type(X, nguoi), dat(X, gau)))
```

**FOL reading**: ∃X (type(X, nguoi) ∧ dat(X, gau))

#### Multiple variables
```prolog
drs([X, Y], [nguoi(X), cho(Y), co(X, Y)])
→ exists(X, exists(Y, and(nguoi(X), and(cho(Y), co(X, Y)))))
```

**FOL reading**: ∃X ∃Y (nguoi(X) ∧ cho(Y) ∧ co(X, Y))

### 3.3 FOL Simplification

```prolog
fol_simplify(and(true, X), X).
fol_simplify(and(X, true), X).
```

Removes redundant `true` from conjunctions.

---

## 4. engine/prover.pl - Theorem Proving

### 4.1 DRS Proving

```prolog
prove_drs(drs(Universe, Conditions)) :-
    prove_conditions(Conditions).
```

The prover uses Prolog's built-in unification and backtracking to find bindings for variables in the Universe that satisfy all Conditions.

### 4.2 Condition Proving

```prolog
prove_single_condition(type(Entity, Type)) :-
    facts:type(Entity, Type).

prove_single_condition(Pred) :-
    call(Pred).
```

**Example**: Proving `drs([X], [type(X, nguoi), dat(X, gau)])`

```prolog
prove_conditions([type(X, nguoi), dat(X, gau)])
```

1. **Prove** `type(X, nguoi)`:
   - Query KB: `facts:type(X, nguoi)`
   - Binds: `X = huy` (from `type(huy, nguoi)`)

2. **Prove** `dat(huy, gau)`:
   - Query KB: `call(dat(huy, gau))`
   - Success: `fact(dat(huy, gau))` ✓

3. **Return**: `X = huy`

### 4.3 FOL Proving

```prolog
prove_fol(exists(Var, Body)) :-
    prove_fol(Body).
```

For existential quantifiers, we just try to prove the body. Prolog's unification will find the witness (binding for Var).

```prolog
prove_fol(and(P, Q)) :-
    prove_fol(P),
    prove_fol(Q).
```

For conjunction, prove both conjuncts.

### 4.4 Who-Question Answering

```prolog
answer_who(drs(Universe, Conditions), Answers) :-
    Universe = [Var|_],
    findall(Var, (
        prove_conditions(Conditions),
        facts:type(Var, nguoi)
    ), Bag),
    sort(Bag, Answers).
```

**Algorithm**:
1. Extract the question variable (first in Universe)
2. Use `findall` to collect all bindings that:
   - Satisfy all conditions
   - AND are of type `nguoi` (person)
3. Sort and return unique answers

**Example**: "Ai dat Gau?"
```prolog
answer_who(drs([X], [type(X, nguoi), dat(X, gau)]), Answers)
→ findall(X, (prove_conditions([...]), type(X, nguoi)), Bag)
→ Answers = [huy]
```

---

## 5. Knowledge Base (kb/facts.pl)

### 5.1 Type System

```prolog
type(huy, nguoi).  % Huy is a person
type(gau, cho).    % Gau is a dog
```

This allows semantic type checking and constraint on question answers.

### 5.2 Facts

```prolog
fact(hien(gau)).        % Gau is gentle
fact(nho(gau)).         % Gau is small
fact(dat(huy, gau)).    % Huy walks Gau
fact(co(huy, gau)).     % Huy has Gau
fact(thich(huy, gau)).  % Huy likes Gau
```

All facts are loaded into the Prolog database by `prover:bootstrap`.

---

## 6. Integration (main.pl)

### 6.1 Pipeline

```prolog
run(Sentence, QuestionType) :-
    tokens(Sentence, Tokens),           % 1. Tokenize
    s(Semantics, Tokens, []),          % 2. Parse + compose
    sem_to_drs(Semantics, DRS),        % 3. Convert to DRS
    drs_to_fol(DRS, FOL),              % 4. Convert to FOL
    prove/answer based on type.        % 5. Execute
```

### 6.2 Question Type Dispatch

```prolog
exec(yn, yn(Sem), DRS) :-
    ( prover:prove_yn(DRS) -> writeln("Yes") ; writeln("No") ).

exec(who, who(_), DRS) :-
    prover:answer_who(DRS, Answers).

exec(what, what(_), DRS) :-
    prover:answer_what(DRS, Answers).
```

---

## 7. Type Theory Perspective

### 7.1 Type Hierarchy

```
t                        % truth values (propositions)
e                        % entities
e → t                    % one-place predicates
e → e → t                % two-place relations
(e → t) → t              % generalized quantifiers (NPs)
```

### 7.2 Lexical Types

| Category | Example | Type | Lambda |
|----------|---------|------|--------|
| Proper Noun | Gau | (e → t) → t | λP.P(gau) |
| Intransitive V | hien | e → t | λX.hien(X) |
| Transitive V | dat | e → e → t | λY.λX.dat(X,Y) |
| Question Word | ai | (e → t) → t | λP.∃X[person(X) ∧ P(X)] |

### 7.3 Composition Types

**Function Application**:
```
If α has type a → b
   β has type a
Then α(β) has type b
```

**Example**: NP + VP
```
NP: (e → t) → t          [λP.P(gau)]
VP: e → t                [λX.hien(X)]
S:  t                    [hien(gau)]
```

---

## 8. Advanced Features

### 8.1 Generalized Quantifiers

The system supports generalized quantifier treatment of NPs:

```prolog
% "cho" as indefinite: ∃Y[cho(Y) ∧ ...]
common_noun(cho, lam(X, cho(X))).

% "Huy co cho" becomes:
% ∃Y[cho(Y) ∧ co(huy, Y)]
```

### 8.2 Scope Ambiguity

Currently, the system has fixed scope:
- Questions always take wide scope
- Quantifiers in object position take narrow scope

**Future work**: Implement Cooper storage for scope ambiguity.

### 8.3 Complex Predicates

Support for multi-argument predicates:

```prolog
% "Long cua Gau mau gi?"
% mau_long(gau, X) - "Gau's fur has color X"
verb_sem(mau_long, lam(Y, lam(X, mau_long(X, Y)))).
```

---

## 9. Debugging Tips

### 9.1 Trace Lambda Reduction

```prolog
?- trace.
?- lexicon:apply(lam(P, app(P, gau)), lam(X, hien(X)), R).
```

### 9.2 Inspect DRS

```prolog
?- grammar:s(Sem, [gau, hien], []).
Sem = yn(hien(gau)).

?- lexicon:sem_to_drs(hien(gau), DRS).
DRS = drs([], [hien(gau)]).
```

### 9.3 Test Prover

```prolog
?- prover:prove_drs(drs([], [hien(gau)])).
true.

?- prover:prove_drs(drs([X], [type(X, nguoi), dat(X, gau)])).
X = huy.
```

---

## 10. Extending the System

### 10.1 Adding New Words

1. **Add to lexicon**:
```prolog
verb_tv_sem(yeu, lam(Y, lam(X, yeu(X, Y)))).
```

2. **Add to grammar**:
```prolog
v_tv(Sem) --> [yeu], { lexicon:verb_tv_sem(yeu, Sem) }.
```

3. **Add facts**:
```prolog
fact(yeu(huy, gau)).
```

### 10.2 Adding New Constructions

Example: Adjective modification

```prolog
% "cho nho" (small dog)
np(Sem) --> adj(AdjSem), common_noun(NounSem), {
    % Intersective semantics: λX[noun(X) ∧ adj(X)]
    Sem = lam(X, and(app(NounSem, X), app(AdjSem, X)))
}.
```

### 10.3 Adding Quantifiers

```prolog
% "mọi" (every)
quantifier(moi, lam(N, lam(P, drs([], [
    forall(X, implies(app(N, X), app(P, X)))
])))).
```

---

This completes the technical documentation. The system is now a fully compositional semantic parser using lambda calculus!
