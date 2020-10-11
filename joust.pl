:- use_module(library(pce)).
:- dynamic user_choice/1.

read_game_config :-
    see(config),
    process_game_config,
    seen.

process_game_config :-
    read(X),
    process_config(X).

process_config(end_of_file) :- !.

process_config(X) :-
    assert(X),
    process_game_config.

start :-
    read_game_config,
    init(Board, WhitePos, BlackPos),
    play(Board, WhitePos, BlackPos, white).

between2(Start, Start, Start) :- !.
between2(Start, End, Start) :- Start @< End.
between2(Start, End, S2) :-
    Start @< End,
    name(Start, [C]),
    C1 is C + 1,
    name(S1, [C1]),
    between2(S1, End, S2).

init_window(Width, Height):-
    free(@p),
    new(@p, picture('Joust')),
    send(@p, ideal_width(Width)),
    send(@p, ideal_height(Height)),
    send(@p, open).

% Generates starting positions
% start(Board, WhitePos, BlackPos).
init(Board, d/1, d/8) :-
    init_window(1920, 1080),
    board_dimensions(XMax, YMax),
    findall(X/Y, (between2(a, XMax, X), between(1, YMax, Y)), Board).

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
    free(@ic),
    new(@ic, device),
    draw_squares(Squares),
    draw_grid(),
    draw_knight(WhitePos, white),
    draw_knight(BlackPos, black),
    send(@p, display, @ic, point(0, 0)).

user_move(M) :-
    repeat,
    (
        retract(user_choice(M)), !
        ;
        sleep(0.2),
        fail
    ).


play(Board, WhitePos, BlackPos, white) :-
    draw_all(Board, WhitePos, BlackPos),
    moves(Board, WhitePos, Moves), Moves \= [], !,
(   repeat,
    user_move(M),
    (
        member(M, Moves), !
        ;
        fail
    )
),
    delete(Board, WhitePos, NewBoard),
    play(NewBoard, M, BlackPos, black).

play(Board, WhitePos, BlackPos, white) :-
    % No moves left
    draw_all(Board, WhitePos, BlackPos),
    new(@Frame, frame('Black wins')),
    send(@Frame, report, inform, 'Black wins!'),
    % Causes problems - debug why.
    % send(@Frame, report, done),
    % Start another game.
    start.

play(Board, WhitePos, BlackPos, black) :-
    draw_all(Board, WhitePos, BlackPos),
    sleep(0.2),
    moves(Board, BlackPos, Moves),
    bestmove(Moves, NewBlackPos), !,
    delete(Board, BlackPos, NewBoard),
    play(NewBoard, WhitePos, NewBlackPos, white).

play(Board, WhitePos, BlackPos, black) :-
    draw_all(Board, WhitePos, BlackPos),
    new(@Frame, frame('White wins')),
    send(@Frame, report, inform, 'White wins!'),
    % Causes problems - debug why.
    %send(@Frame, report, done),
    % Start another game.
    start.



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

square_colour(X/Y, brown) :-
    ord(X, Ord),
    (Ord + Y) mod 2 =:= 0, !.

square_colour(_, yellow).

knight_pic_file_name(X/Y, white, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "images/white_brown.jpg", Path),
    square_colour(X/Y, brown).

knight_pic_file_name(X/Y, black, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "images/black_brown.jpg", Path),
    square_colour(X/Y, brown).

knight_pic_file_name(X/Y, white, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "images/white_yellow.jpg", Path),
    square_colour(X/Y, yellow).

knight_pic_file_name(X/Y, black, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "images/black_yellow.jpg", Path),
    square_colour(X/Y, yellow).

draw_knight(X/Y, Turn) :-
    ord(X, Ord),
    GX is Ord * 100,
    GY is Y * -100,
    knight_pic_file_name(X/Y, Turn, FileName),
    send(@ic, display, new(_, bitmap(FileName)), point(GX, GY)).

assert_user_choice(X, Y) :-
    assertz(user_choice(X/Y)).

draw_vertical_grid([]):-!.
draw_vertical_grid([Y|Tail]):-
    new(@Bo, box(40, 100)),
    GY is Y * -100,
    send(@Bo, x(60)),
    send(@Bo, y(GY)),
    send(@Bo, fill_pattern, colour(white)),
    send(@ic, display, @Bo),
    send(@ic, display, new(T, text(Y, center, bold))),
    send(T, center, @Bo?center),
    draw_vertical_grid(Tail).

draw_horizontal_grid([]):-!.
draw_horizontal_grid([X|Tail]):-
    new(@Bo, box(100, 40)),
    ord(X, Ord),
    GX is Ord * 100,
    send(@Bo, x(GX)),
    send(@Bo, y(0)),
    send(@Bo, fill_pattern, colour(white)),
    send(@ic, display, @Bo),
    send(@ic, display, new(T, text(X, center, bold))),
    send(T, center, @Bo?center),
    draw_horizontal_grid(Tail).

draw_grid :-
    findall(X, between2(a, h, X), Horizontal),
    findall(Y, between(1, 8, Y), Vertical),
    draw_vertical_grid(Vertical),
    draw_horizontal_grid(Horizontal).
    

draw_squares([X/Y|Tail]) :-
    new(@Bo, box(100, 100)),
    ord(X, Ord),
    GX is Ord * 100,
    GY is Y * -100,
    send(@Bo, x(GX)),
    send(@Bo, y(GY)),
    square_colour(X/Y, Colour),
    send(@Bo, fill_pattern, colour(Colour)),
    send(@ic, display, @Bo),
    send(@Bo, recogniser, click_gesture(left, '', single, message(@prolog, assert_user_choice, X, Y))),
    draw_squares(Tail).

draw_squares([]).













