# Visual Architecture Diagrams

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    LAMBDA CALCULUS SEMANTIC PARSER              │
└─────────────────────────────────────────────────────────────────┘

INPUT: "Gau hien khong?"
   │
   ├──► [TOKENIZER] ──► [gau, hien, khong]
   │
   ├──► [PARSER + LAMBDA COMPOSITION]
   │       │
   │       ├── Lexicon: gau → λP.P(gau)
   │       ├── Lexicon: hien → λX.hien(X)
   │       └── Grammar: S → NP VP
   │                    Apply + Beta Reduce
   │
   ├──► [LAMBDA EXPRESSION] ──► yn(hien(gau))
   │
   ├──► [DRS BUILDER] ──► drs([], [hien(gau)])
   │
   ├──► [FOL CONVERTER] ──► hien(gau)
   │
   └──► [THEOREM PROVER] ──► YES ✓
```

## Lambda Composition Flow

```
Example: "Gau hien" (Gau is gentle)

┌─────────────┐
│     NP      │  λP.P(gau)
│    "Gau"    │  Type: (e→t)→t
└──────┬──────┘
       │
       │ APPLY to
       │
┌──────▼──────┐
│     VP      │  λX.hien(X)
│   "hien"    │  Type: e→t
└──────┬──────┘
       │
       │ BETA REDUCTION
       │
       ▼
   (λP.P(gau))(λX.hien(X))
       │
       │ Step 1: substitute P
       ▼
   (λX.hien(X))(gau)
       │
       │ Step 2: substitute X
       ▼
   hien(gau)
       
   ✓ FINAL RESULT
```

## Type System Hierarchy

```
                    ┌─────┐
                    │  t  │  (Truth values)
                    └─────┘
                       ▲
                       │
        ┌──────────────┼──────────────┐
        │              │              │
    ┌───┴───┐      ┌───┴───┐     ┌───┴────┐
    │ e → t │      │e→e→t  │     │(e→t)→t │
    └───────┘      └───────┘     └────────┘
        │              │              │
        │              │              │
    Properties     Relations    Quantifiers
    (hien)         (dat)        (ai, gi, NPs)
```

## Module Interaction

```
┌──────────────────────────────────────────────────────────┐
│                        main.pl                           │
│  • Initialization                                        │
│  • Pipeline orchestration                                │
└────────┬──────────────────────┬──────────────────────────┘
         │                      │
         ▼                      ▼
┌─────────────────┐    ┌─────────────────┐
│  grammar.pl     │    │  lexicon.pl     │
│                 │◄───┤                 │
│ • DCG rules     │    │ • Lambda terms  │
│ • Composition   │    │ • Beta reduce   │
│                 │    │ • Apply         │
└────────┬────────┘    └─────────────────┘
         │
         ├─────────┬─────────┬────────────┐
         ▼         ▼         ▼            ▼
    ┌────────┐ ┌──────┐ ┌─────────┐ ┌─────────┐
    │ fol.pl │ │prover│ │facts.pl │ │ data/   │
    │        │ │  .pl │ │         │ │         │
    │DRS→FOL │ │Prove │ │   KB    │ │Questions│
    └────────┘ └──────┘ └─────────┘ └─────────┘
```

## Question Processing Pipeline

### Yes/No Question: "Gau hien khong?"

```
┌─────────────────────────────────────────────┐
│ 1. TOKENIZE                                 │
│    "Gau hien khong" → [gau, hien, khong]   │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 2. PARSE                                    │
│    s(yn(Sem)) --> np(NPSem), vp(VPSem)     │
│                                             │
│    NP: λP.P(gau)                           │
│    VP: λX.hien(X)                          │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 3. LAMBDA COMPOSITION                       │
│    apply(λP.P(gau), λX.hien(X))            │
│    = (λP.P(gau))(λX.hien(X))               │
│    → (λX.hien(X))(gau)                     │
│    → hien(gau)                              │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 4. DRS CONSTRUCTION                         │
│    yn(hien(gau))                           │
│    → drs([], [hien(gau)])                  │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 5. FOL CONVERSION                           │
│    drs([], [hien(gau)])                    │
│    → hien(gau)                              │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 6. THEOREM PROVING                          │
│    prove_yn(drs([], [hien(gau)]))          │
│    → call(hien(gau))                       │
│    → fact(hien(gau)) ✓                     │
│                                             │
│    ANSWER: YES ✓                           │
└─────────────────────────────────────────────┘
```

### Who Question: "Ai dat Gau?"

```
┌─────────────────────────────────────────────┐
│ 1. TOKENIZE                                 │
│    "Ai dat Gau" → [ai, dat, gau]           │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 2. PARSE                                    │
│    s(who(Sem)) --> [ai], vp(VPSem)         │
│                                             │
│    Q: λP.drs([X], [type(X,nguoi), P(X)])  │
│    VP: λX.dat(X, gau)                      │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 3. LAMBDA COMPOSITION                       │
│    apply(Q, VP)                             │
│    = drs([X], [type(X,nguoi),              │
│              (λX.dat(X,gau))(X)])          │
│    → drs([X], [type(X,nguoi),              │
│               dat(X,gau)])                  │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 4. DRS (already constructed)                │
│    who(drs([X], [type(X,nguoi),            │
│                  dat(X,gau)]))              │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 5. FOL CONVERSION                           │
│    drs([X], [...])                          │
│    → ∃X (type(X,nguoi) ∧ dat(X,gau))      │
└────────────────┬────────────────────────────┘
                 │
┌────────────────▼────────────────────────────┐
│ 6. ANSWER FINDING                           │
│    findall(X, (                             │
│        type(X, nguoi),                      │
│        dat(X, gau)                          │
│    ), Answers)                              │
│                                             │
│    KB Query:                                │
│    • type(huy, nguoi) ✓                    │
│    • dat(huy, gau) ✓                       │
│                                             │
│    ANSWER: [huy]                           │
└─────────────────────────────────────────────┘
```

## Beta Reduction Process

```
Example: (λX.hien(X))(gau)

┌──────────────────────────────────────┐
│ Initial: app(lam(X, hien(X)), gau)  │
└───────────────┬──────────────────────┘
                │
                ▼
┌──────────────────────────────────────┐
│ Match pattern:                       │
│   app(lam(Var, Body), Arg)          │
│                                      │
│ Bindings:                            │
│   Var = X                            │
│   Body = hien(X)                     │
│   Arg = gau                          │
└───────────────┬──────────────────────┘
                │
                ▼
┌──────────────────────────────────────┐
│ Call: substitute(X, gau, hien(X), R)│
└───────────────┬──────────────────────┘
                │
                ▼
┌──────────────────────────────────────┐
│ Substitute X with gau in hien(X)    │
│                                      │
│ Pattern: hien(X)                     │
│ Match: X = X, replace with gau       │
│ Result: hien(gau)                    │
└───────────────┬──────────────────────┘
                │
                ▼
┌──────────────────────────────────────┐
│ Final Result: hien(gau) ✓           │
└──────────────────────────────────────┘
```

## DRS Structure

```
DRS: drs(Universe, Conditions)

Example: "Ai dat Gau?" (Who walks Gau?)

┌────────────────────────────────────┐
│        DRS Structure               │
│                                    │
│  ┌──────────────────────────────┐ │
│  │ Universe: [X]                │ │ ← Discourse referents
│  └──────────────────────────────┘ │
│                                    │
│  ┌──────────────────────────────┐ │
│  │ Conditions:                  │ │
│  │   • type(X, nguoi)          │ │ ← X is a person
│  │   • dat(X, gau)             │ │ ← X walks Gau
│  └──────────────────────────────┘ │
│                                    │
└────────────────────────────────────┘

Semantics: ∃X (type(X, nguoi) ∧ dat(X, gau))

Reading: "There exists an X such that
          X is a person AND X walks Gau"
```

## Knowledge Base Structure

```
┌─────────────────────────────────────────┐
│           facts.pl (KB)                 │
├─────────────────────────────────────────┤
│                                         │
│  TYPE SYSTEM                            │
│  ┌───────────────────────────────┐     │
│  │ type(huy, nguoi)              │     │
│  │ type(gau, cho)                │     │
│  └───────────────────────────────┘     │
│                                         │
│  FACTS (KNOWLEDGE)                      │
│  ┌───────────────────────────────┐     │
│  │ fact(hien(gau))               │     │
│  │ fact(nho(gau))                │     │
│  │ fact(dat(huy, gau))           │     │
│  │ fact(co(huy, gau))            │     │
│  │ fact(thich(huy, gau))         │     │
│  │ fact(thich(gau, huy))         │     │
│  │ fact(mau_long(gau, nau))      │     │
│  │ fact(ten(gau, gau))           │     │
│  └───────────────────────────────┘     │
│                                         │
└─────────────────────────────────────────┘

        ▲
        │ Query
        │
┌───────┴─────────┐
│  Theorem Prover │
│                 │
│  • prove_yn     │
│  • answer_who   │
│  • answer_what  │
└─────────────────┘
```

## Complete Example Trace: "Ai dat Gau?"

```
INPUT STRING
     │
     ▼
"Ai dat Gau"
     │
     ▼ tokenize
[ai, dat, gau]
     │
     ▼ parse
Grammar matches: s(who(Sem)) --> [ai], vp(VPSem)
     │
     ├─► [ai] → Q-word
     │         λP.drs([X], [type(X, nguoi), P(X)])
     │
     └─► vp → dat Gau
             │
             ├─► v_tv: dat → λY.λX.dat(X, Y)
             └─► np: Gau → λP.P(gau)
             │
             Apply & Reduce:
             (λY.λX.dat(X,Y))(λP.P(gau))
             → ... → λX.dat(X, gau)
     │
     ▼ compose Q-word + VP
apply(λP.drs([X], [type(X,nguoi), P(X)]), 
      λX.dat(X, gau))
     │
     ▼ beta reduce
drs([X], [type(X, nguoi), (λX.dat(X,gau))(X)])
     │
     ▼ further reduce
drs([X], [type(X, nguoi), dat(X, gau)])
     │
     ▼ to FOL
∃X (type(X, nguoi) ∧ dat(X, gau))
     │
     ▼ answer
findall(X, (type(X, nguoi), dat(X, gau)), Bag)
     │
     ├─► Query KB: type(X, nguoi)
     │             → X = huy
     │
     └─► Verify: dat(huy, gau)
                 → fact(dat(huy, gau)) ✓
     │
     ▼
RESULT: [huy]
```

---

These diagrams illustrate the complete flow of the lambda calculus-based semantic parser!
