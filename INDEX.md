# 📚 Documentation Index

## Start Here!

**New to the project?** Start with:
1. [README.md](README.md) - Project overview and quick start
2. [LAMBDA_CALCULUS_README.md](LAMBDA_CALCULUS_README.md) - Complete system explanation
3. [STEP_BY_STEP_EXAMPLES.md](STEP_BY_STEP_EXAMPLES.md) - See it in action!

## 📖 Documentation Files

### 🌟 Main Documentation

| File | Purpose | Audience | Reading Time |
|------|---------|----------|--------------|
| [README.md](README.md) | Project overview, quick start, examples | Everyone | 10 min |
| [LAMBDA_CALCULUS_README.md](LAMBDA_CALCULUS_README.md) | Complete system guide with theory | Students/Researchers | 30 min |
| [TECHNICAL_GUIDE.md](TECHNICAL_GUIDE.md) | Deep implementation details | Developers | 45 min |
| [STEP_BY_STEP_EXAMPLES.md](STEP_BY_STEP_EXAMPLES.md) | Worked examples with traces | Learners | 20 min |
| [QUICK_REFERENCE.md](QUICK_REFERENCE.md) | Quick lookup guide | Developers | 5 min |
| [ARCHITECTURE_DIAGRAMS.md](ARCHITECTURE_DIAGRAMS.md) | Visual system diagrams | Visual learners | 15 min |
| [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md) | Implementation overview | Project managers | 10 min |

### 📂 By Purpose

#### For Learning Lambda Calculus
1. **[LAMBDA_CALCULUS_README.md](LAMBDA_CALCULUS_README.md)** - Start here for theory
2. **[STEP_BY_STEP_EXAMPLES.md](STEP_BY_STEP_EXAMPLES.md)** - See examples
3. **[ARCHITECTURE_DIAGRAMS.md](ARCHITECTURE_DIAGRAMS.md)** - Visualize concepts
4. **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - Quick syntax lookup

#### For Understanding the Code
1. **[TECHNICAL_GUIDE.md](TECHNICAL_GUIDE.md)** - Module-by-module breakdown
2. **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - Function reference
3. Source code files (see below)

#### For Using the System
1. **[README.md](README.md)** - Installation and running
2. **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - Commands and patterns
3. **test/test_examples.pl** - Example queries

#### For Extending the System
1. **[TECHNICAL_GUIDE.md](TECHNICAL_GUIDE.md)** Section 10 - Extension patterns
2. **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - "Common Patterns" section
3. **[README.md](README.md)** - "Extending the System" section

## 💻 Source Code Files

### Core Implementation

| File | Lines | Purpose |
|------|-------|---------|
| **main.pl** | ~60 | Entry point and pipeline |
| **semantics/lexicon.pl** | ~150 | Lambda expressions & beta reduction |
| **syntax/grammar.pl** | ~120 | DCG rules with semantic composition |
| **engine/fol.pl** | ~60 | DRS to FOL conversion |
| **engine/prover.pl** | ~120 | Theorem prover |
| **kb/facts.pl** | ~30 | Knowledge base |

### Testing

| File | Lines | Purpose |
|------|-------|---------|
| **test/test_examples.pl** | ~200 | Comprehensive test suite |

### Data

| File | Purpose |
|------|---------|
| **data/passage.txt** | Context passage |
| **data/questions.json** | Test questions |

## 🗺️ Reading Paths

### Path 1: Quick Start (30 minutes)
```
README.md (10 min)
    ↓
STEP_BY_STEP_EXAMPLES.md (20 min)
    ↓
Try running the code!
```

### Path 2: Deep Understanding (2 hours)
```
README.md (10 min)
    ↓
LAMBDA_CALCULUS_README.md (30 min)
    ↓
STEP_BY_STEP_EXAMPLES.md (20 min)
    ↓
TECHNICAL_GUIDE.md (45 min)
    ↓
Read source code with QUICK_REFERENCE.md
```

### Path 3: Visual Learner (1 hour)
```
README.md (10 min)
    ↓
ARCHITECTURE_DIAGRAMS.md (15 min)
    ↓
STEP_BY_STEP_EXAMPLES.md (20 min)
    ↓
LAMBDA_CALCULUS_README.md (30 min)
```

### Path 4: Developer Quick Start (45 minutes)
```
README.md (10 min)
    ↓
QUICK_REFERENCE.md (5 min)
    ↓
TECHNICAL_GUIDE.md (30 min)
    ↓
Start coding!
```

## 📋 Key Topics by Document

### Lambda Calculus Concepts
- **Theory**: LAMBDA_CALCULUS_README.md §1, TECHNICAL_GUIDE.md §1
- **Syntax**: QUICK_REFERENCE.md "Lambda Term Syntax"
- **Examples**: STEP_BY_STEP_EXAMPLES.md all sections
- **Diagrams**: ARCHITECTURE_DIAGRAMS.md "Lambda Composition Flow"

### Beta Reduction
- **Algorithm**: TECHNICAL_GUIDE.md §1.3-1.4
- **Examples**: STEP_BY_STEP_EXAMPLES.md §1.4, §2.4
- **Reference**: QUICK_REFERENCE.md "Key Functions"
- **Diagrams**: ARCHITECTURE_DIAGRAMS.md "Beta Reduction Process"

### DRS (Discourse Representation Structures)
- **Theory**: LAMBDA_CALCULUS_README.md §4
- **Construction**: TECHNICAL_GUIDE.md §1.5
- **Examples**: STEP_BY_STEP_EXAMPLES.md all sections
- **Diagrams**: ARCHITECTURE_DIAGRAMS.md "DRS Structure"

### Semantic Composition
- **Theory**: LAMBDA_CALCULUS_README.md §2
- **Implementation**: TECHNICAL_GUIDE.md §2
- **Examples**: STEP_BY_STEP_EXAMPLES.md §1.3, §2.3
- **Patterns**: QUICK_REFERENCE.md "Common Compositions"

### Theorem Proving
- **Theory**: LAMBDA_CALCULUS_README.md §7
- **Implementation**: TECHNICAL_GUIDE.md §4
- **Examples**: STEP_BY_STEP_EXAMPLES.md §1.7, §2.7
- **Reference**: QUICK_REFERENCE.md "Prover Operations"

## 🎯 FAQs and Where to Find Answers

### "How does lambda calculus work?"
→ **LAMBDA_CALCULUS_README.md** §1 + **STEP_BY_STEP_EXAMPLES.md**

### "How do I add a new word?"
→ **QUICK_REFERENCE.md** "Adding New Word" + **TECHNICAL_GUIDE.md** §10.1

### "What are the types of expressions?"
→ **TECHNICAL_GUIDE.md** §7 + **QUICK_REFERENCE.md** "Type Signatures"

### "How does beta reduction work?"
→ **TECHNICAL_GUIDE.md** §1.3 + **ARCHITECTURE_DIAGRAMS.md** "Beta Reduction Process"

### "How are questions answered?"
→ **STEP_BY_STEP_EXAMPLES.md** Examples 2-3 + **TECHNICAL_GUIDE.md** §4.4

### "How do I test the system?"
→ **README.md** "Testing" + **test/test_examples.pl**

### "What's the architecture?"
→ **ARCHITECTURE_DIAGRAMS.md** + **README.md** "Architecture"

### "How do I extend the grammar?"
→ **TECHNICAL_GUIDE.md** §10.2-10.3

## 📊 Documentation Statistics

- **Total documentation**: ~4,000 lines
- **Code examples**: 50+ snippets
- **Worked examples**: 4 complete traces
- **Diagrams**: 10+ visual diagrams
- **Tables**: 15+ reference tables
- **Test cases**: 8+ query types

## 🔍 Search Guide

### Find Information About...

**Lambda expressions**: 
- LAMBDA_CALCULUS_README.md §1
- QUICK_REFERENCE.md "Lexical Categories"

**Grammar rules**: 
- TECHNICAL_GUIDE.md §2
- QUICK_REFERENCE.md "Grammar Rules Pattern"

**DRS**: 
- LAMBDA_CALCULUS_README.md §4
- TECHNICAL_GUIDE.md §1.5
- QUICK_REFERENCE.md "DRS Structure"

**FOL conversion**: 
- LAMBDA_CALCULUS_README.md §5
- TECHNICAL_GUIDE.md §3
- QUICK_REFERENCE.md "FOL Conversion"

**Prover**: 
- LAMBDA_CALCULUS_README.md §7
- TECHNICAL_GUIDE.md §4
- QUICK_REFERENCE.md "Prover Operations"

**Testing**: 
- README.md "Testing"
- test/test_examples.pl

**Examples**: 
- STEP_BY_STEP_EXAMPLES.md (all)
- LAMBDA_CALCULUS_README.md §2

**Type system**: 
- TECHNICAL_GUIDE.md §7
- QUICK_REFERENCE.md "Type Signatures Summary"

## 🎓 Academic Use

### For Teaching
Use in order:
1. README.md - Introduction
2. ARCHITECTURE_DIAGRAMS.md - Visual overview
3. LAMBDA_CALCULUS_README.md - Theory
4. STEP_BY_STEP_EXAMPLES.md - Practice

### For Research
Key sections:
- TECHNICAL_GUIDE.md - Implementation details
- LAMBDA_CALCULUS_README.md §10 - Future work
- Source code - Actual implementation

### For Projects
Start with:
- QUICK_REFERENCE.md - Quick lookup
- TECHNICAL_GUIDE.md §10 - Extension guide
- test/test_examples.pl - Testing framework

## 📞 Need Help?

1. **Quick question?** → Check QUICK_REFERENCE.md
2. **Understanding concept?** → Read LAMBDA_CALCULUS_README.md
3. **Implementation detail?** → Check TECHNICAL_GUIDE.md
4. **See an example?** → Check STEP_BY_STEP_EXAMPLES.md
5. **Visual explanation?** → Check ARCHITECTURE_DIAGRAMS.md

---

**Happy Learning!** 🎉

Start with [README.md](README.md) and follow your preferred reading path above.
