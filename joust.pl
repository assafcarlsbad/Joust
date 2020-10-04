:- use_module(library(pce)).

start :-
    init(Board, WhitePos, BlackPos),
    play(Board, WhitePos, BlackPos, white).

% Generates starting positions
% start(Board, WhitePos, BlackPos).
init(Board, d/1, d/8) :-
    Board = [
            a/1, a/2, a/3, a/4, a/5, a/6, a/7, a/8,
            b/1, b/2, b/3, b/4, b/5, b/6, b/7, b/8,
            c/1, c/2, c/3, c/4, c/5, c/6, c/7, c/8,
            d/1, d/2, d/3, d/4, d/5, d/6, d/7, d/8,
            e/1, e/2, e/3, e/4, e/5, e/6, e/7, e/8,
            f/1, f/2, f/3, f/4, f/5, f/6, f/7, f/8,
            g/1, g/2, g/3, g/4, g/5, g/6, g/7, g/8,
            h/1, h/2, h/3, h/4, h/5, h/6, h/7, h/8].

next_char(C, C1) :-
    name(C, [X|_]),
    X1 is X + 1,
    name(C1, [X1]).

prev_char(C, C1) :-
    name(C, [X|_]),
    X1 is X - 1,
    name(C1, [X1]).

% For now just pick the first one
bestmove([X|_], X).

draw_all(Squares, WhitePos, BlackPos) :-
    free(@p),
    new(@p, picture('Joust')),
    send(@p, ideal_width(1920)),
    send(@p, ideal_height(1080)),
    draw_squares(Squares),
    draw_knight(WhitePos, white),
    draw_knight(BlackPos, black),
    send(@p, open).

play(Board, WhitePos, BlackPos, white) :-
    draw_all(Board, WhitePos, BlackPos),
    %sleep(2),
    moves(Board, WhitePos, Moves),
    bestmove(Moves, NewWhitePos), !,
    delete(Board, WhitePos, NewBoard),
    play(NewBoard, NewWhitePos, BlackPos, black).

play(Board, WhitePos, BlackPos, white) :-
    % No moves left    new(@Frame, frame('Black wins')),
    send(@Frame, report, inform, 'Black wins!'),
    send(@Frame, report, done),
    draw_all(Board, WhitePos, BlackPos),
    !.
    %write('Black wins'), !.

play(Board, WhitePos, BlackPos, black) :-
    draw_all(Board, WhitePos, BlackPos),
    %sleep(2),
    moves(Board, BlackPos, Moves),
    bestmove(Moves, NewBlackPos), !,
    delete(Board, BlackPos, NewBoard),
    play(NewBoard, WhitePos, NewBlackPos, white).

play(Board, WhitePos, BlackPos, black) :-
    new(@Frame, frame('White wins')),
    send(@Frame, report, inform, 'White wins!'),
    send(@Frame, report, done),
    draw_all(Board, WhitePos, BlackPos),
    !.
%    write('White wins'), !.


% Generates a list of legal moves for the white
moves(Board, X/Y, L) :-
    findall(NewPos, (move(X/Y, NewPos), member(NewPos, Board)), L).

% Move North-East
move(X/Y, X1/Y1) :-
    next_char(X, X1),
    Y1 is Y + 2.

% Move North-West
move(X/Y, X1/Y1) :-
    prev_char(X, X1),
    Y1 is Y + 2.

% Move West-North
move(X/Y, X2/Y1) :-
    prev_char(X, X1),
    prev_char(X1, X2),
    Y1 is Y + 1.

% Move West-South
move(X/Y, X2/Y1) :-
    prev_char(X, X1),
    prev_char(X1, X2),
    Y1 is Y - 1.

% Move South-West
move(X/Y, X1/Y1) :-
    prev_char(X, X1),
    Y1 is Y - 2.

% Move South-East
move(X/Y, X1/Y1) :-
    next_char(X, X1),
    Y1 is Y - 2.

% Move East-North
move(X/Y, X2/Y1) :-
    next_char(X, X1),
    next_char(X1, X2),
    Y1 is Y + 1.

% Move East-South
move(X/Y, X2/Y1) :-
    next_char(X, X1),
    next_char(X1, X2),
    Y1 is Y - 1.

ord(Char, Ord) :-
    name(Char, [Ord1|_]),
    Ord is Ord1 - 96.

draw_knight(X/Y, white) :-
    ord(X, Ord),
    GX is Ord * 100,
    GY is Y * -100,
    send(@p, display, new(@Bitmap, bitmap('C:/white.jpg')), point(GX, GY)).

draw_knight(X/Y, black) :-
    ord(X, Ord),
    GX is Ord * 100,
    GY is Y * -100,
    send(@p, display, new(@Bitmap, bitmap('C:/black.jpg')), point(GX, GY)).



square_colour(X/Y, brown) :-
    ord(X, Ord),
    (Ord + Y) mod 2 =:= 0, !.

square_colour(_, yellow).

draw_squares([X/Y|Tail]) :-
    new(@Bo, box(100, 100)),
    ord(X, Ord),
    GX is Ord * 100,
    GY is Y * -100,
    send(@Bo, x(GX)),
    send(@Bo, y(GY)),
    square_colour(X/Y, Colour),
    send(@Bo, fill_pattern, colour(Colour)),
    send(@p, display, @Bo),
    draw_squares(Tail).

draw_squares([]).
