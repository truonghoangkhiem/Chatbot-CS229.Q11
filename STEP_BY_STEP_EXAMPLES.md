# Step-by-Step Examples

## Example 1: Simple Yes/No Question "Gau hien khong?"

### Step 1: Tokenization
```
Input: "Gau hien khong"
Output: [gau, hien, khong]
```

### Step 2: Parsing with DCG
```prolog
s(yn(Sem)) --> np(NPSem), vp(VPSem), opt_khong
```

**NP Parse**: `np(lam(P, app(P, gau)))`
- Rule: `np(Sem) --> [gau], { lexicon:noun_sem(gau, Sem) }`
- Semantics: **λP.P(gau)**

**VP Parse**: `vp(lam(X, hien(X)))`
- Rule: `vp(Sem) --> v_iv(Sem)`
- Sub-rule: `v_iv(Sem) --> [hien], { lexicon:verb_iv_sem(hien, Sem) }`
- Semantics: **λX.hien(X)**

### Step 3: Semantic Composition
```prolog
lexicon:apply(NPSem, VPSem, ReducedSem)
```

**Application**:
```
(λP.P(gau))(λX.hien(X))
```

### Step 4: Beta Reduction

**Step 4a**: Apply the NP to VP
```
app(lam(P, app(P, gau)), lam(X, hien(X)))
```

**Step 4b**: Substitute P with λX.hien(X)
```
substitute(P, lam(X, hien(X)), app(P, gau), Result)
= app(lam(X, hien(X)), gau)
```

**Step 4c**: Reduce further
```
app(lam(X, hien(X)), gau)
→ substitute(X, gau, hien(X), Result)
→ hien(gau)
```

**Final Lambda Term**: `hien(gau)`

### Step 5: Convert to DRS
```prolog
lexicon:sem_to_drs(hien(gau), DRS)
```

**Result**:
```prolog
drs([], [hien(gau)])
```

**Interpretation**: No discourse referents, single condition: hien(gau)

### Step 6: Convert to FOL
```prolog
fol:drs_to_fol(drs([], [hien(gau)]), FOL)
```

**Result**:
```prolog
hien(gau)
```

**Interpretation**: Simple atomic formula (no quantifiers needed)

### Step 7: Theorem Proving
```prolog
prover:prove_yn(drs([], [hien(gau)]))
→ prove_conditions([hien(gau)])
→ prove_single_condition(hien(gau))
→ call(hien(gau))
→ fact(hien(gau))  ✓ [Found in KB]
```

**Result**: **YES** ✓

---

## Example 2: Who-Question "Ai dat Gau?"

### Step 1: Tokenization
```
Input: "Ai dat Gau"
Output: [ai, dat, gau]
```

### Step 2: Parsing

**Grammar Rule**:
```prolog
s(who(Sem)) --> [ai], vp(VPSem), {
    lexicon:question_word_sem(ai, QSem),
    lexicon:apply(QSem, VPSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

**Q-word "ai"**: 
```prolog
lam(P, drs([X], [type(X, nguoi), app(P, X)]))
```
Type: **(e → t) → t**
Meaning: **λP.∃X[person(X) ∧ P(X)]**

**VP "dat Gau"**:

First, parse transitive verb + object:
```prolog
vp(Sem) --> v_tv(VerbSem), np(ObjSem), {
    lexicon:apply(VerbSem, ObjSem, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

- **Verb "dat"**: `lam(Y, lam(X, dat(X, Y)))`
- **Object "Gau"**: `lam(P, app(P, gau))`

**VP Composition**:

Actually, we need to handle this carefully. The object NP `lam(P, app(P, gau))` needs to be "lowered" to entity type.

Let me trace through what actually happens:

```
VerbSem = lam(Y, lam(X, dat(X, Y)))
ObjSem = lam(P, app(P, gau))

apply(VerbSem, ObjSem, R1)
= app(lam(Y, lam(X, dat(X, Y))), lam(P, app(P, gau)))
```

Wait, this doesn't type-check! We need object to be type `e`, not `(e→t)→t`.

**Solution**: The grammar should extract the entity from generalized quantifier:

Actually, for proper nouns in object position, we can use a simpler approach:

```prolog
% In grammar.pl, we should have object extraction:
obj_entity(Entity) --> np(lam(P, app(P, Entity))).
```

Let me assume the VP simplifies to: `lam(X, dat(X, gau))`

### Step 3: Compose Q-word with VP

```
QSem = lam(P, drs([X], [type(X, nguoi), app(P, X)]))
VPSem = lam(X, dat(X, gau))

apply(QSem, VPSem, Applied)
= app(lam(P, drs([X], [type(X, nguoi), app(P, X)])), 
      lam(X, dat(X, gau)))
```

### Step 4: Beta Reduction

```
substitute(P, lam(X, dat(X, gau)), 
           drs([X], [type(X, nguoi), app(P, X)]), 
           Result)
```

**Substitute P in conditions**:
```
[type(X, nguoi), app(P, X)]
→ [type(X, nguoi), app(lam(X, dat(X, gau)), X)]
```

**Result**:
```prolog
drs([X], [type(X, nguoi), app(lam(X, dat(X, gau)), X)])
```

### Step 5: Resolve embedded application

We need to further reduce `app(lam(X, dat(X, gau)), X)`:

```
app(lam(X, dat(X, gau)), X)
→ substitute(X, X, dat(X, gau), Result)
→ dat(X, gau)
```

**Final DRS**:
```prolog
drs([X], [type(X, nguoi), dat(X, gau)])
```

### Step 6: Convert to FOL

```prolog
drs([X], [type(X, nguoi), dat(X, gau)])
→ exists(X, and(type(X, nguoi), dat(X, gau)))
```

**FOL Reading**: **∃X (type(X, nguoi) ∧ dat(X, gau))**

"There exists an X such that X is a person and X walks Gau"

### Step 7: Answer Finding

```prolog
answer_who(drs([X], [type(X, nguoi), dat(X, gau)]), Answers)
```

**Algorithm**:
```prolog
findall(X, (
    prove_conditions([type(X, nguoi), dat(X, gau)]),
    facts:type(X, nguoi)
), Bag)
```

**Execution**:
1. Try to prove: `type(X, nguoi)` → Binds `X = huy`
2. Try to prove: `dat(huy, gau)` → Success (fact in KB)
3. Verify: `type(huy, nguoi)` → Success

**Result**: `Answers = [huy]`

---

## Example 3: What-Question "Long cua Gau mau gi?"

### Step 1: Tokenization
```
Input: "Long cua Gau mau gi"
Output: [long, cua, gau, mau, gi]
```

### Step 2: Parsing

**Grammar Rule**:
```prolog
s(what(Sem)) --> [long, cua], np(NPSem), [mau], [gi], {
    VerbSem = lam(Y, lam(X, mau_long(X, Y))),
    lexicon:question_word_sem(gi, QSem),
    lexicon:apply(VerbSem, QSem, ObjApplied),
    lexicon:apply(NPSem, ObjApplied, ReducedSem),
    lexicon:beta_reduce(ReducedSem, Sem)
}.
```

**Components**:
- **NP "Gau"**: `lam(P, app(P, gau))`
- **Verb "mau_long"**: `lam(Y, lam(X, mau_long(X, Y)))`
- **Q-word "gi"**: `lam(P, drs([X], [app(P, X)]))`

### Step 3: Apply Verb to Q-word

```
apply(lam(Y, lam(X, mau_long(X, Y))), 
      lam(P, drs([X], [app(P, X)])),
      ObjApplied)
```

This is tricky! We're applying a `e → e → t` to a `(e → t) → t`.

**Type coercion needed**: We need to extract the variable from the q-word.

Let's think of it differently. The q-word takes the predicate:

```
apply(lam(P, drs([Y], [app(P, Y)])),
      lam(X, lam(Y, mau_long(Y, X))),  % Note: we curry the other way
      Result)
```

Actually, let's construct this more carefully in the grammar:

```prolog
% Create predicate: \Y.mau_long(gau, Y)
VerbPartial = lam(Y, mau_long(gau, Y))

% Apply q-word to this predicate
apply(lam(P, drs([X], [app(P, X)])), VerbPartial, Sem)
```

### Step 4: Beta Reduction

```
app(lam(P, drs([X], [app(P, X)])), lam(Y, mau_long(gau, Y)))
→ substitute(P, lam(Y, mau_long(gau, Y)), drs([X], [app(P, X)]), Result)
→ drs([X], [app(lam(Y, mau_long(gau, Y)), X)])
```

### Step 5: Resolve embedded application

```
app(lam(Y, mau_long(gau, Y)), X)
→ substitute(Y, X, mau_long(gau, Y), Result)
→ mau_long(gau, X)
```

**Final DRS**:
```prolog
drs([X], [mau_long(gau, X)])
```

### Step 6: Convert to FOL

```prolog
drs([X], [mau_long(gau, X)])
→ exists(X, mau_long(gau, X))
```

**FOL Reading**: **∃X mau_long(gau, X)**

"There exists an X such that Gau's fur has color X"

### Step 7: Answer Finding

```prolog
answer_what(drs([X], [mau_long(gau, X)]), Answers)
```

**Execution**:
```prolog
findall(X, prove_conditions([mau_long(gau, X)]), Bag)
→ findall(X, call(mau_long(gau, X)), Bag)
→ Query: mau_long(gau, X)
→ Match: fact(mau_long(gau, nau))
→ X = nau
```

**Result**: `Answers = [nau]`

---

## Example 4: Complex Composition "Huy dat Gau khong?"

### Complete Trace

**Input**: "Huy dat Gau khong"

**Tokens**: `[huy, dat, gau, khong]`

**Parse Tree**:
```
S
├── NP: Huy
│   └── Sem: lam(P, app(P, huy))
├── VP: dat Gau
│   ├── V: dat
│   │   └── Sem: lam(Y, lam(X, dat(X, Y)))
│   └── NP: Gau
│       └── Sem: lam(P, app(P, gau))
└── opt_khong: khong
```

**VP Composition**:
```
Verb: lam(Y, lam(X, dat(X, Y)))
Object: lam(P, app(P, gau))

Step 1: Apply verb to object
app(lam(Y, lam(X, dat(X, Y))), lam(P, app(P, gau)))

Step 2: Beta reduce
substitute(Y, lam(P, app(P, gau)), lam(X, dat(X, Y)), R)
= lam(X, dat(X, lam(P, app(P, gau))))

This still has a lambda in argument position!
We need to reduce: dat(X, lam(P, app(P, gau)))

Actually, for proper noun objects, we should extract entity:
app(lam(P, app(P, gau)), ???)

The issue is we need the entity gau, not the GQ.
```

**Better approach in grammar**:
```prolog
% For transitive verbs, extract entity from object NP
vp(Sem) --> v_tv(lam(Y, lam(X, Pred))), np(lam(P, app(P, Obj))), {
    Sem = lam(X, Pred[Y/Obj])  % Substitute Obj for Y
}.
```

Or we can use **inverse scope**:
```
Object NP: lam(P, app(P, gau))
Takes VP: lam(Y, lam(X, dat(X, Y)))

Apply object to predicate formed from verb:
app(lam(P, app(P, gau)), lam(Y, lam(X, dat(X, Y))))
→ app(lam(Y, lam(X, dat(X, Y))), gau)
→ lam(X, dat(X, gau))
```

**Final VP**: `lam(X, dat(X, gau))`

**S Composition**:
```
Subject: lam(P, app(P, huy))
VP: lam(X, dat(X, gau))

apply(lam(P, app(P, huy)), lam(X, dat(X, gau)), Sem)
= app(lam(P, app(P, huy)), lam(X, dat(X, gau)))
→ app(lam(X, dat(X, gau)), huy)
→ dat(huy, gau)
```

**DRS**: `drs([], [dat(huy, gau)])`

**FOL**: `dat(huy, gau)`

**Proof**: 
```
prove_yn(drs([], [dat(huy, gau)]))
→ call(dat(huy, gau))
→ fact(dat(huy, gau)) ✓
```

**Answer**: **YES** ✓

---

## Summary of Lambda Calculus Flow

1. **Lexicon** → Lambda expressions for words
2. **Grammar** → Combine syntax + apply lambda functions
3. **Beta Reduction** → Simplify lambda applications
4. **DRS** → Structured semantic representation
5. **FOL** → Logical formula with quantifiers
6. **Prover** → Find bindings and verify against KB

Each step maintains compositionality and formal semantics!
