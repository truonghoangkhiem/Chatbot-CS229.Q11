# Lambda Calculus-based Semantic Analysis System

## Tổng quan

Hệ thống này đã được nâng cấp để sử dụng **Lambda Calculus** và **Discourse Representation Structures (DRS)** cho phân tích ngữ nghĩa. Thay vì chỉ ghép nối các term đơn giản, hệ thống hiện thực hiện **semantic composition** thông qua phép toán lambda.

## Kiến trúc hệ thống

```
┌─────────────────┐
│  Input String   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Tokenization   │  (grammar.pl)
└────────┬────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Parsing + Lambda Composition   │  (grammar.pl + lexicon.pl)
│                                 │
│  • DCG rules combine syntax     │
│  • Lambda expressions compose   │
│  • Beta reduction computes      │
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────┐
│  Lambda Term    │  (e.g., hien(gau))
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  DRS Structure  │  (lexicon.pl)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  FOL Formula    │  (fol.pl)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Theorem Prover │  (prover.pl)
└─────────────────┘
```

## 1. Lambda Expressions trong Lexicon (semantics/lexicon.pl)

### Danh từ riêng (Proper Nouns)
```prolog
% Gau: \P.P(gau)
noun_sem(gau, lam(P, app(P, gau))).

% Huy: \P.P(huy)
noun_sem(huy, lam(P, app(P, huy))).
```

**Ý nghĩa**: Một danh từ riêng là một hàm nhận một thuộc tính `P` và áp dụng thuộc tính đó cho entity (gau hoặc huy).

### Động từ nội (Intransitive Verbs)
```prolog
% hien: \X.hien(X)
verb_iv_sem(hien, lam(X, hien(X))).

% nho: \X.nho(X)
verb_iv_sem(nho, lam(X, nho(X))).
```

**Ý nghĩa**: Một động từ nội là một hàm nhận một thực thể `X` và khẳng định thuộc tính đó về X.

### Động từ ngoại (Transitive Verbs)
```prolog
% dat: \Y.\X.dat(X, Y)
verb_tv_sem(dat, lam(Y, lam(X, dat(X, Y)))).

% thich: \Y.\X.thich(X, Y)
verb_tv_sem(thich, lam(Y, lam(X, thich(X, Y)))).

% co: \Y.\X.co(X, Y)
verb_tv_sem(co, lam(Y, lam(X, co(X, Y)))).
```

**Ý nghĩa**: Một động từ ngoại là một hàm curried:
- Đầu tiên nhận đối tượng `Y`
- Sau đó nhận chủ ngữ `X`
- Cuối cùng khẳng định quan hệ `verb(X, Y)`

### Từ hỏi (Question Words)
```prolog
% ai (who): \P.drs([X], [type(X, nguoi), P(X)])
question_word_sem(ai, lam(P, drs([X], [type(X, nguoi), app(P, X)]))).

% gi (what): \P.drs([X], [P(X)])
question_word_sem(gi, lam(P, drs([X], [app(P, X)]))).
```

**Ý nghĩa**: Từ hỏi tạo ra DRS với:
- Universe: chứa biến discourse referent `X`
- Conditions: các điều kiện về `X`

## 2. Semantic Composition trong Grammar (syntax/grammar.pl)

### Ví dụ: "Gau hien" (Gấu hiền)

1. **NP (Gau)**: `\P.P(gau)`
2. **VP (hien)**: `\X.hien(X)`
3. **Quy tắc S → NP VP**:
   ```prolog
   s(yn(Sem)) --> np(NPSem), vp(VPSem), opt_khong, {
       lexicon:apply(NPSem, VPSem, ReducedSem),
       lexicon:beta_reduce(ReducedSem, Sem)
   }.
   ```

4. **Beta Reduction**:
   ```
   apply(\P.P(gau), \X.hien(X))
   = (\P.P(gau))(\X.hien(X))
   = (\X.hien(X))(gau)     [substitute P with \X.hien(X)]
   = hien(gau)              [substitute X with gau]
   ```

### Ví dụ: "Huy dat Gau" (Huy dắt Gấu)

1. **NP (Huy)**: `\P.P(huy)`
2. **V (dat)**: `\Y.\X.dat(X, Y)`
3. **NP (Gau)**: `\P.P(gau)`
4. **VP = V + Object**:
   ```
   apply(\Y.\X.dat(X, Y), \P.P(gau))
   = (\Y.\X.dat(X, Y))(\P.P(gau))
   = \X.dat(X, (\P.P(gau)))      [substitute Y with \P.P(gau)]
   
   Nhưng ta cần evaluate (\P.P(gau)) applied to gau
   Thực tế VP construction cần phức tạp hơn...
   ```

5. **S = NP + VP**:
   ```
   apply(\P.P(huy), \X.dat(X, gau))
   = dat(huy, gau)
   ```

### Ví dụ: "Ai dat Gau?" (Ai dắt Gấu?)

1. **Q-word (ai)**: `\P.drs([X], [type(X, nguoi), P(X)])`
2. **VP (dat Gau)**: `\X.dat(X, gau)`
3. **Composition**:
   ```
   apply(\P.drs([X], [type(X, nguoi), P(X)]), \X.dat(X, gau))
   = drs([X], [type(X, nguoi), (\X.dat(X, gau))(X)])
   = drs([X], [type(X, nguoi), dat(X, gau)])
   ```

## 3. Beta Reduction (semantics/lexicon.pl)

### Thuật toán

```prolog
% Main reduction rule
beta_reduce(app(lam(Var, Body), Arg), Result) :-
    substitute(Var, Arg, Body, Result).

% Substitution: Replace Var with Value in Expression
substitute(Var, Value, Var, Value).          % Base case: found variable
substitute(Var, Value, lam(Var, Body), lam(Var, Body)).  % Bound variable
substitute(Var, Value, lam(Other, Body), lam(Other, NewBody)) :-
    substitute(Var, Value, Body, NewBody).   % Substitute in lambda body
substitute(Var, Value, app(F, A), app(NewF, NewA)) :-
    substitute(Var, Value, F, NewF),
    substitute(Var, Value, A, NewA).         % Substitute in application
```

### Ví dụ thực thi

```
beta_reduce(app(lam(X, hien(X)), gau), Result)
→ substitute(X, gau, hien(X), Result)
→ Result = hien(gau)
```

## 4. DRS Construction

### Cấu trúc DRS
```prolog
drs(Universe, Conditions)
```

- **Universe**: Danh sách các discourse referents (biến)
- **Conditions**: Danh sách các điều kiện/mệnh đề

### Ví dụ

**Câu**: "Ai dat Gau?"

**DRS**:
```prolog
drs([X], [type(X, nguoi), dat(X, gau)])
```

Ý nghĩa: "Tồn tại X sao cho X là người và X dắt Gấu"

## 5. DRS to FOL Conversion (engine/fol.pl)

### Quy tắc chuyển đổi

```prolog
drs([X, Y], [P(X), Q(X, Y)]) 
→ exists(X, exists(Y, and(P(X), Q(X, Y))))
```

### Ví dụ

```prolog
drs([X], [type(X, nguoi), dat(X, gau)])
→ exists(X, and(type(X, nguoi), dat(X, gau)))
```

**Đọc là**: "∃X (type(X, nguoi) ∧ dat(X, gau))"

## 6. Theorem Prover (engine/prover.pl)

### Chức năng

Prover bây giờ là một **theorem prover** thực sự:

1. **Nhận DRS/FOL**: Thay vì mệnh đề đơn giản
2. **Tìm bindings**: Tìm các giá trị cho biến thỏa mãn điều kiện
3. **Kiểm tra với KB**: Đối chiếu với knowledge base (facts.pl)

### Thuật toán cho DRS

```prolog
prove_drs(drs(Universe, Conditions)) :-
    % Tìm các bindings cho tất cả biến trong Universe
    % sao cho tất cả Conditions đều đúng
    prove_conditions(Conditions).

prove_conditions([Cond|Rest]) :-
    prove_single_condition(Cond),
    prove_conditions(Rest).
```

### Ví dụ

**Query**: "Ai dat Gau?"

**DRS**: `drs([X], [type(X, nguoi), dat(X, gau)])`

**Prover thực thi**:
```prolog
prove_conditions([type(X, nguoi), dat(X, gau)])
→ type(X, nguoi)  [X = huy từ KB]
→ dat(huy, gau)   [Kiểm tra KB: fact(dat(huy, gau)) ✓]
→ Success, X = huy
```

## 7. Cách chạy hệ thống

### Yêu cầu
- SWI-Prolog

### Chạy
```bash
swipl -g main -t halt main.pl
```

### Kết quả mong đợi

```
=== Gau hien khong ===
Tokens: [gau,hien,khong]
Lambda Semantics: yn(hien(gau))
DRS: drs([], [hien(gau)])
FOL: hien(gau)
👉 Yes

=== Ai dat Gau ===
Tokens: [ai,dat,gau]
Lambda Semantics: who(drs([X], [type(X, nguoi), dat(X, gau)]))
DRS: drs([X], [type(X, nguoi), dat(X, gau)])
FOL: exists(X, and(type(X, nguoi), dat(X, gau)))
👉 Who = [huy]
```

## 8. Tính năng nâng cao

### Type Checking
Hệ thống sử dụng `type(Entity, Type)` để đảm bảo semantic correctness:
- `type(huy, nguoi)`: Huy là người
- `type(gau, cho)`: Gấu là chó

### Conjunction Handling
"Huy co cho" → `drs([Y], [co(huy, Y), cho(Y)])`
Nghĩa là: "Huy có một Y nào đó là chó"

### Question Processing
- **Who-questions**: Trả về entities có type `nguoi`
- **What-questions**: Trả về bất kỳ entity nào thỏa mãn

## 9. So sánh với hệ thống cũ

| Khía cạnh | Hệ thống cũ | Hệ thống mới (Lambda) |
|-----------|-------------|----------------------|
| Ngữ nghĩa từ vựng | Atom đơn giản (`gau`) | Lambda expression (`\P.P(gau)`) |
| Composition | Ghép nối trực tiếp | Beta reduction |
| Biểu diễn ngữ nghĩa | Mệnh đề Prolog | DRS → FOL |
| Prover | Simple `call/1` | Theorem prover với quantifiers |
| Xử lý biến | Template với `arg/2` | Discourse referents trong DRS |
| Tính compositional | Thấp | Cao (principled composition) |

## 10. Mở rộng trong tương lai

1. **Quantifiers**: Thêm "tất cả" (forall)
2. **Negation**: Xử lý phủ định trong DRS
3. **Complex NPs**: "con chó của Huy"
4. **Anaphora Resolution**: Xử lý đại từ nhân xưng
5. **Temporal Logic**: Xử lý thời gian
6. **Modal Logic**: "có thể", "phải"

## Kết luận

Hệ thống đã được nâng cấp hoàn toàn với:
✅ Lambda calculus-based semantics
✅ Beta reduction mechanism
✅ DRS construction
✅ FOL conversion
✅ Theorem prover for DRS/FOL

Đây là một hệ thống phân tích ngữ nghĩa compositional đầy đủ, tuân theo các nguyên lý của formal semantics hiện đại.
