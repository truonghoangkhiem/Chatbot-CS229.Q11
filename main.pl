:- initialization(main, main).
:- use_module(syntax/grammar).
:- use_module(semantics/lexicon).
:- use_module(engine/fol).
:- use_module(engine/prover).

main :-
    prover:bootstrap,
    writeln("================================================="),
    writeln("   HE THONG HOI DAP - CS229.Q11 (BAO CAO DO AN)"),
    writeln("================================================="),
    
    writeln("\n--- PHAN 1: CAU HOI DON (TABLE 3: 1-9) ---"),
    % STT 1: Huy có chó không? -> Yes
    run("Huy co cho khong", yn),
    
    % STT 2: Gấu hiền không? -> Yes
    run("Gau hien khong", yn),
    
    % STT 3: Gấu nhỏ không? -> Yes
    run("Gau nho khong", yn),
    
    % STT 4: Gấu lớn không? -> No
    run("Gau lon khong", yn),
    
    % STT 5: Ai dắt Gấu? -> Huy
    run("Ai dat Gau", who),
    
    % STT 6: Ai thích Gấu? [Kho - Can luat suy dien doi xung] -> Huy
    run("Ai thich Gau", who),
    
    % STT 7: Huy có gì? -> Gấu
    run("Huy co gi", what),
    
    % STT 8: Lông của Gấu màu gì? -> Nâu
    run("Long cua Gau mau gi", what),
    
    % STT 9: Con chó tên gì? -> Gấu
    run("Con cho ten gi", what),
    
    writeln("\n--- PHAN 2: CAU HOI CO NGU CANH (TABLE 3: 10-12) ---"),
    writeln("(Minh hoa co che DRS & Dong so chi dai tu)"),
    
    % STT 10: Huy có một con chó. Nó hiền không? -> Yes
    run_context(["Huy co mot con cho", "No hien khong"], yn),
    
    % STT 11: Huy dắt Gấu. Nó nhỏ không? -> Yes
    run_context(["Huy dat Gau", "No nho khong"], yn),
    
    % STT 12: Huy có một con chó. Lông của nó màu gì? -> Nâu
    run_context(["Huy co mot con cho", "Long cua no mau gi"], what),

    halt.

% --- Xử lý câu đơn ---
run(Sent, Kind) :-
    grammar:tokens(Sent, Toks),
    ( grammar:s(Sem, Toks, []) ->
        format("~n[INPUT]: ~s~n", [Sent]),
        sem_to_drs_wrapper(Kind, Sem, DRS),
        fol:drs_to_fol(DRS, FOL),
        format("  FOL: ~w~n", [FOL]),
        (Kind=yn -> (prover:prove_yn(DRS) -> A='Yes'; A='No');
         Kind=who -> prover:answer_who(DRS, A);
         prover:answer_what(DRS, A)),
        format("  >> KET QUA: ~w~n", [A])
    ;   format("Parse Failed: ~s~n", [Sent])
    ).

% --- Xử lý ngữ cảnh (2 câu) theo Báo cáo Mục 9 ---
run_context([Sent1, Sent2], Kind) :-
    format("~n[CONTEXT]: ~s. ~s~n", [Sent1, Sent2]),
    
    % BƯỚC 1: Xử lý câu 1 (Câu trần thuật)
    grammar:tokens(Sent1, Toks1),
    grammar:s(Sem1Raw, Toks1, []),
    
    % Unwrap semantics
    ( Sem1Raw = decl(Sem1) -> true
    ; Sem1Raw = yn(Sem1) -> true
    ; Sem1 = Sem1Raw
    ),
    
    % Chuyển sang DRS để tìm referent (thực thể)
    ( Sem1 = drs(_, _) -> 
        lexicon:resolve_drs(Sem1, DRS1)
    ; 
        lexicon:sem_to_drs(Sem1, DRS1_Raw),
        lexicon:resolve_drs(DRS1_Raw, DRS1)
    ),
    
    % Xử lý Đồng sở chỉ (Anaphora Resolution) như mô tả mục 9.3
    % "Khi gặp đại từ 'nó', chọn referent gần nhất phù hợp kiểu"
    ( DRS1 = drs([_|_], _) ->
        % Trường hợp 1: Câu 1 giới thiệu biến mới (vd: một con chó -> X)
        prover:answer_what(DRS1, [ResolvedRef|_]),
        format("  [INFO] Dong so chi: 'No' -> ~w (Tim thay tu DRS S1)~n", [ResolvedRef])
    ; 
        % Trường hợp 2: Câu 1 dùng tên riêng (vd: Huy dắt Gấu)
        % Heuristic: Lấy thực thể không phải là người (Huy) để gán cho 'Nó'
        grammar:tokens(Sent1, T1), 
        member(ResolvedRef, [gau, huy]), 
        member(ResolvedRefStr, T1), 
        atom_string(ResolvedRef, ResolvedRefStr),
        ResolvedRef \= huy, 
        format("  [INFO] Dong so chi: 'No' -> ~w (Trich xuat tu S1)~n", [ResolvedRef])
    ),

    % BƯỚC 2: Xử lý câu 2 (Câu hỏi chứa 'REF')
    grammar:tokens(Sent2, Toks2),
    grammar:s(Sem2, Toks2, []),
    
    % Thay thế placeholder 'REF' bằng ResolvedRef (Gau) TRƯỚC KHI resolve
    sem_to_drs_wrapper_context(Kind, Sem2, ResolvedRef, DRS2),
    
    fol:drs_to_fol(DRS2, FOL),
    format("  FOL (S2): ~w~n", [FOL]),

    % BƯỚC 3: Trả lời dựa trên câu 2 đã được giải nghĩa đầy đủ
    (Kind=yn -> (prover:prove_yn(DRS2) -> A='Yes'; A='No');
     Kind=who -> prover:answer_who(DRS2, A);
     prover:answer_what(DRS2, A)),
    format("  >> KET QUA: ~w~n", [A]).


% --- Helper wrappers ---
sem_to_drs_wrapper(yn, yn(S), D) :- 
    (S=lam(_,_) -> lexicon:beta_reduce(S, R), lexicon:sem_to_drs(R, D0); lexicon:sem_to_drs(S, D0)), 
    lexicon:resolve_drs(D0, D).

sem_to_drs_wrapper(who, who(S), D) :- 
    (S=drs(_,_) -> D0=S; lexicon:sem_to_drs(S, D0)), 
    lexicon:resolve_drs(D0, D).

sem_to_drs_wrapper(what, what(S), D) :-
    ( S = lam(Var, drs(U, C)) -> D0 = drs([Var|U], C)
    ; S = lam(Var, Body) -> term_variables(Body, _), D0 = drs([Var], [Body])
    ; D0 = S ),
    lexicon:resolve_drs(D0, D).

% Context wrappers: Thay thế REF trước
sem_to_drs_wrapper_context(yn, yn(S), Ref, D) :- 
    (S=lam(_,_) -> lexicon:beta_reduce(S, R), lexicon:sem_to_drs(R, D0); lexicon:sem_to_drs(S, D0)), 
    lexicon:replace_ref('REF', Ref, D0, D1),
    lexicon:resolve_drs(D1, D).

sem_to_drs_wrapper_context(who, who(S), Ref, D) :- 
    (S=drs(_,_) -> D0=S; lexicon:sem_to_drs(S, D0)), 
    lexicon:replace_ref('REF', Ref, D0, D1),
    lexicon:resolve_drs(D1, D).

sem_to_drs_wrapper_context(what, what(S), Ref, D) :-
    ( S = lam(Var, drs(U, C)) -> D0 = drs([Var|U], C)
    ; S = lam(Var, Body) -> D0 = drs([Var], [Body])
    ; D0 = S ),
    lexicon:replace_ref('REF', Ref, D0, D1),
    lexicon:resolve_drs(D1, D).