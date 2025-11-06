# Quick Reference: Lambda Calculus Implementation

## Lambda Term Syntax

| Construct | Prolog Representation | Mathematical Notation | Example |
|-----------|----------------------|----------------------|---------|
| Lambda abstraction | `lam(X, Body)` | λX.Body | `lam(X, hien(X))` = λX.hien(X) |
| Application | `app(Func, Arg)` | Func(Arg) | `app(lam(X, f(X)), a)` = (λX.f(X))(a) |
| Variable | `X` | X | `X` |
| Constant | `atom` | atom | `gau`, `huy` |

## Lexical Categories

### Proper Nouns (Type: (e→t)→t)
```prolog
noun_sem(gau, lam(P, app(P, gau))).
noun_sem(huy, lam(P, app(P, huy))).
```
**Meaning**: λP.P(entity) - "Apply any property P to this entity"

### Intransitive Verbs (Type: e→t)
```prolog
verb_iv_sem(hien, lam(X, hien(X))).
verb_iv_sem(nho, lam(X, nho(X))).
```
**Meaning**: λX.property(X) - "X has this property"

### Transitive Verbs (Type: e→e→t)
```prolog
verb_tv_sem(dat, lam(Y, lam(X, dat(X, Y)))).
verb_tv_sem(thich, lam(Y, lam(X, thich(X, Y)))).
verb_tv_sem(co, lam(Y, lam(X, co(X, Y)))).
```
**Meaning**: λY.λX.relation(X,Y) - "X bears this relation to Y"

### Question Words
```prolog
% who: (e→t)→t
question_word_sem(ai, lam(P, drs([X], [type(X, nguoi), app(P, X)]))).

% what: (e→t)→t  
question_word_sem(gi, lam(P, drs([X], [app(P, X)]))).
```

## Key Functions

### Beta Reduction
```prolog
beta_reduce(app(lam(Var, Body), Arg), Result)
```
**Computes**: (λVar.Body)(Arg) → Body[Var/Arg]

**Example**:
```prolog
?- beta_reduce(app(lam(X, hien(X)), gau), R).
R = hien(gau).
```

### Application
```prolog
apply(Function, Argument, Result)
```
**Wrapper** for application + beta reduction

### Substitution
```prolog
substitute(Variable, Value, Expression, Result)
```
**Replaces** all free occurrences of Variable with Value in Expression

## DRS Structure

```prolog
drs(Universe, Conditions)
```

- **Universe**: List of discourse referents (variables)
- **Conditions**: List of predicates/formulas

**Examples**:
```prolog
drs([], [hien(gau)])                              % Simple fact
drs([X], [type(X, nguoi), dat(X, gau)])          % Who question
drs([X, Y], [nguoi(X), cho(Y), co(X, Y)])        % Multiple referents
```

## Grammar Rules Pattern

```prolog
grammatical_category(Semantics) --> 
    sub_category1(Sem1), 
    sub_category2(Sem2), 
    {
        % Semantic composition
        lexicon:apply(Sem1, Sem2, Applied),
        lexicon:beta_reduce(Applied, Semantics)
    }.
```

## Common Compositions

### S → NP VP (Yes/No)
```prolog
NP: lam(P, app(P, entity))      % e.g., gau
VP: lam(X, predicate(X))        % e.g., hien
S:  predicate(entity)           % Result: hien(gau)
```

### VP → V NP (Transitive)
```prolog
V:  lam(Y, lam(X, rel(X, Y)))   % e.g., dat
NP: lam(P, app(P, obj))         % e.g., gau
VP: lam(X, rel(X, obj))         % Result: dat(X, gau)
```

### S → Q-word VP (Who-question)
```prolog
Q:  lam(P, drs([X], [type(X, nguoi), app(P, X)]))  % ai
VP: lam(X, predicate(X))                            % dat(X, gau)
S:  drs([X], [type(X, nguoi), predicate(X)])       % Result
```

## FOL Conversion

| DRS | FOL |
|-----|-----|
| `drs([], [P])` | `P` |
| `drs([], [P, Q])` | `and(P, Q)` |
| `drs([X], [P(X)])` | `exists(X, P(X))` |
| `drs([X, Y], [P(X), Q(X,Y)])` | `exists(X, exists(Y, and(P(X), Q(X,Y))))` |

## Prover Operations

### Yes/No Questions
```prolog
prove_yn(drs(Universe, Conditions))
```
**Returns**: true if all conditions can be proven

### Who Questions
```prolog
answer_who(drs([X|_], Conditions), Answers)
```
**Returns**: List of entities (type: nguoi) satisfying conditions

### What Questions
```prolog
answer_what(drs([X|_], Conditions), Answers)
```
**Returns**: List of all entities satisfying conditions

## Testing Commands

```prolog
% Load system
?- [main].

% Bootstrap KB
?- prover:bootstrap.

% Parse a sentence
?- grammar:tokens("Gau hien khong", Toks),
   grammar:s(Sem, Toks, []).

% Test lambda reduction
?- lexicon:beta_reduce(app(lam(X, f(X)), a), R).

% Convert to DRS
?- lexicon:sem_to_drs(hien(gau), DRS).

% Convert to FOL
?- fol:drs_to_fol(drs([X], [P(X)]), FOL).

% Prove
?- prover:prove_yn(drs([], [hien(gau)])).

% Answer who
?- prover:answer_who(drs([X], [type(X, nguoi), dat(X, gau)]), Ans).
```

## Common Patterns

### Adding New Word

1. **Add to lexicon.pl**:
```prolog
verb_tv_sem(yeu, lam(Y, lam(X, yeu(X, Y)))).
```

2. **Add to grammar.pl**:
```prolog
v_tv(Sem) --> [yeu], { lexicon:verb_tv_sem(yeu, Sem) }.
```

3. **Add to facts.pl**:
```prolog
fact(yeu(huy, gau)).
```

### Debugging Lambda Reduction

```prolog
% Trace mode
?- trace.
?- beta_reduce(Expression, Result).

% Show intermediate steps
?- beta_reduce(app(lam(X, f(X)), a), R),
   format("Result: ~w~n", [R]).
```

## Type Signatures Summary

```
Entity:                e
Truth value:           t
Property:              e → t
Binary relation:       e → e → t
Generalized quantifier: (e → t) → t
Question operator:      (e → t) → t
```

## Files Overview

| File | Purpose |
|------|---------|
| `semantics/lexicon.pl` | Lambda expressions, beta reduction |
| `syntax/grammar.pl` | DCG rules, semantic composition |
| `engine/fol.pl` | DRS to FOL conversion |
| `engine/prover.pl` | Theorem proving, question answering |
| `kb/facts.pl` | Knowledge base |
| `main.pl` | Main entry point |

## Workflow

```
Input String
    ↓ tokenize
[Token, List]
    ↓ parse (DCG)
Lambda Expression
    ↓ beta_reduce
Simplified Lambda
    ↓ sem_to_drs
DRS Structure
    ↓ drs_to_fol
FOL Formula
    ↓ prove/answer
Result
```

---

**Remember**: Every grammar rule performs BOTH syntactic and semantic composition!
