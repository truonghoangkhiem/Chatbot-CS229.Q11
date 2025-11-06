# Chatbot-CS229.Q11: Lambda Calculus Semantic Parser

A compositional semantic parser for Vietnamese using **Lambda Calculus**, **Discourse Representation Structures (DRS)**, and **First-Order Logic (FOL)**.

## 🎯 Overview

This system implements a **formal compositional semantics** approach to natural language understanding, where:

1. **Words** are represented as **lambda expressions**
2. **Syntax** drives **semantic composition** through **beta reduction**
3. **Meaning** is represented as **DRS** (Discourse Representation Structures)
4. **Reasoning** is performed via **theorem proving** on **FOL** (First-Order Logic)

## 🏗️ Architecture

```
┌──────────────┐
│ Input String │ "Gau hien khong?"
└──────┬───────┘
       │ tokenization
       ▼
┌──────────────┐
│ Token List   │ [gau, hien, khong]
└──────┬───────┘
       │ parsing + lambda composition
       ▼
┌──────────────────────────┐
│ Lambda Expression        │ yn(hien(gau))
│ lam(P, app(P, gau))     │
│ ⊗ lam(X, hien(X))       │ ← Beta Reduction
│ = hien(gau)              │
└──────┬───────────────────┘
       │ DRS construction
       ▼
┌──────────────────────────┐
│ DRS Structure            │ drs([], [hien(gau)])
└──────┬───────────────────┘
       │ FOL conversion
       ▼
┌──────────────────────────┐
│ First-Order Logic        │ hien(gau)
└──────┬───────────────────┘
       │ theorem proving
       ▼
┌──────────────────────────┐
│ Answer                   │ Yes ✓
└──────────────────────────┘
```

## 📁 Project Structure

```
Chatbot-CS229.Q11/
├── main.pl                      # Entry point
├── data/
│   ├── passage.txt              # Context passage
│   └── questions.json           # Test questions
├── semantics/
│   └── lexicon.pl              # Lambda expressions & beta reduction
├── syntax/
│   └── grammar.pl              # DCG rules with semantic composition
├── engine/
│   ├── fol.pl                  # DRS to FOL conversion
│   └── prover.pl               # Theorem prover
├── kb/
│   └── facts.pl                # Knowledge base
├── test/
│   └── test_examples.pl        # Test suite
└── docs/
    ├── LAMBDA_CALCULUS_README.md    # Complete overview
    ├── TECHNICAL_GUIDE.md            # Deep technical details
    ├── STEP_BY_STEP_EXAMPLES.md     # Worked examples
    └── QUICK_REFERENCE.md            # Quick reference
```

## 🚀 Quick Start

### Prerequisites

- SWI-Prolog (version 8.0+)

### Installation

```bash
# Install SWI-Prolog (Ubuntu/Debian)
sudo apt-get install swi-prolog

# Or on macOS
brew install swi-prolog
```

### Running

```bash
# Run all demo queries
swipl -g main -t halt main.pl

# Or interactive mode
swipl
?- [main].
?- prover:bootstrap.
?- run("Gau hien khong", yn).
```

### Testing

```bash
swipl
?- [test/test_examples].
?- test_examples:test_all.
```

## 📚 Example Queries

### Yes/No Questions

```prolog
?- run("Gau hien khong", yn).
=== Gau hien khong ===
Tokens: [gau,hien,khong]
Lambda Semantics: yn(hien(gau))
DRS: drs([],[hien(gau)])
FOL: hien(gau)
👉 Yes
```

### Who Questions

```prolog
?- run("Ai dat Gau", who).
=== Ai dat Gau ===
Tokens: [ai,dat,gau]
Lambda Semantics: who(drs([X],[type(X,nguoi),dat(X,gau)]))
DRS: drs([X],[type(X,nguoi),dat(X,gau)])
FOL: exists(X,and(type(X,nguoi),dat(X,gau)))
👉 Who = [huy]
```

### What Questions

```prolog
?- run("Long cua Gau mau gi", what).
=== Long cua Gau mau gi ===
Tokens: [long,cua,gau,mau,gi]
Lambda Semantics: what(drs([X],[mau_long(gau,X)]))
DRS: drs([X],[mau_long(gau,X)])
FOL: exists(X,mau_long(gau,X))
👉 What = [nau]
```

## 🔬 Key Innovations

### 1. Lambda Calculus for Lexical Semantics

Each word is a typed lambda expression:

```prolog
% Proper noun: λP.P(entity)
noun_sem(gau, lam(P, app(P, gau))).

% Intransitive verb: λX.property(X)
verb_iv_sem(hien, lam(X, hien(X))).

% Transitive verb: λY.λX.relation(X,Y)
verb_tv_sem(dat, lam(Y, lam(X, dat(X, Y)))).
```

### 2. Compositional Semantics via Beta Reduction

Grammar rules perform semantic composition:

```prolog
s(yn(Sem)) --> np(NPSem), vp(VPSem), {
    lexicon:apply(NPSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

Example: "Gau hien"
```
(λP.P(gau))(λX.hien(X))
→ (λX.hien(X))(gau)
→ hien(gau)
```

### 3. DRS for Discourse Representation

Complex questions generate DRS:

```prolog
% "Ai dat Gau?" → Who walks Gau?
drs([X], [type(X, nguoi), dat(X, gau)])
```

**Meaning**: ∃X (X is a person ∧ X walks Gau)

### 4. Theorem Proving for QA

Prover finds variable bindings satisfying conditions:

```prolog
prove_drs(drs([X], [type(X, nguoi), dat(X, gau)]))
→ Finds X = huy from knowledge base
```

## 📖 Documentation

- **[LAMBDA_CALCULUS_README.md](LAMBDA_CALCULUS_README.md)** - Complete system overview
- **[TECHNICAL_GUIDE.md](TECHNICAL_GUIDE.md)** - Implementation details for each module
- **[STEP_BY_STEP_EXAMPLES.md](STEP_BY_STEP_EXAMPLES.md)** - Worked examples with full traces
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - Quick lookup for syntax and functions

## 🎓 Theoretical Background

This system implements concepts from:

- **Lambda Calculus** (Church, 1936)
- **Montague Semantics** (Montague, 1973)
- **Discourse Representation Theory** (Kamp, 1981)
- **Type-Driven Semantics** (Klein & Sag, 1985)

### Type System

```
t                        Truth values
e                        Entities
e → t                    Properties (1-place predicates)
e → e → t                Relations (2-place predicates)
(e → t) → t              Generalized quantifiers (NPs)
```

## 🔧 Extending the System

### Adding New Words

1. Add lambda expression in `semantics/lexicon.pl`:
```prolog
verb_tv_sem(yeu, lam(Y, lam(X, yeu(X, Y)))).
```

2. Add grammar rule in `syntax/grammar.pl`:
```prolog
v_tv(Sem) --> [yeu], { lexicon:verb_tv_sem(yeu, Sem) }.
```

3. Add facts in `kb/facts.pl`:
```prolog
fact(yeu(huy, gau)).
```

### Adding New Constructions

See `TECHNICAL_GUIDE.md` section 10 for examples of adding:
- Adjective modification
- Quantifiers (every, some, all)
- Relative clauses
- Temporal expressions

## 🧪 Testing

Run the test suite:

```prolog
?- [test/test_examples].
?- test_examples:test_all.        % All tests
?- test_examples:test_yn.         % Yes/No tests only
?- test_examples:test_who.        % Who-question tests
?- test_examples:test_what.       % What-question tests
```

## 📊 Current Coverage

**Question Types**:
- ✅ Yes/No questions
- ✅ Who questions
- ✅ What questions
- ⬜ When questions (future)
- ⬜ Why questions (future)

**Linguistic Phenomena**:
- ✅ Proper nouns
- ✅ Intransitive verbs
- ✅ Transitive verbs
- ✅ Simple questions
- ⬜ Quantifiers (future)
- ⬜ Negation (future)
- ⬜ Relative clauses (future)

## 🤝 Contributing

This is an academic project. For improvements:

1. Add more Vietnamese vocabulary in lexicon
2. Implement additional grammatical constructions
3. Extend DRS to handle anaphora
4. Add temporal and modal logic

## 📄 License

Educational/Academic use.

## 👥 Authors

CS229.Q11 Course Project

## 🙏 Acknowledgments

Built upon principles from:
- Blackburn & Bos: "Representation and Inference for Natural Language" (2005)
- Heim & Kratzer: "Semantics in Generative Grammar" (1998)
- Kamp & Reyle: "From Discourse to Logic" (1993)

---

**Note**: This system demonstrates formal semantic composition using lambda calculus. While currently focused on Vietnamese question-answering, the architecture is language-independent and can be extended to other languages and domains.
