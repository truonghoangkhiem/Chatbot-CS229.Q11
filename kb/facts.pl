:- module(facts, [fact/1, type/2, world_rule/1]).

% ===== Thực thể & kiểu =====
type(huy, nguoi).
type(gau, cho).

% ===== Sự kiện từ đoạn văn =====
fact(co(huy, gau)).                 % Huy có (một con chó) Gấu
fact(cho(gau)).                      % Gấu là chó
fact(nho(gau)).                      % Con chó đó nhỏ
fact(ten(gau, gau)).                 % Tên là "Gấu" (đặt atom 'gau' để đơn giản)
fact(mau_long(gau, nau)).            % Bộ lông màu nâu
fact(hien(gau)).                     % Gấu hiền
fact(dat(huy, gau)).                 % Huy dắt Gấu
fact(thich(huy, gau)).               % Huy thích Gấu

% ===== Luật thế giới (để trống tạm) =====
world_rule(thich(X,Y)) :- fact(thich(Y,X)).
