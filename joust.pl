:- use_module(library(pce)).
:- use_module(moves).
:- use_module(ai).
:- dynamic user_choice/1.

search_depth(1) :-
    difficulty(easy).
search_depth(2) :-
    difficulty(medium).
search_depth(3) :-
    difficulty(hard).
search_depth(4) :-
    difficulty(ultra).

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
    play(state(Board, WhitePos, BlackPos, white)).

init_window(Width, Height):-
    free(@p),
    new(@p, picture('Joust')),
    send(@p, ideal_width(Width)),
    send(@p, ideal_height(Height)),
    send(@p, open).

% Generates starting positions
% start(Board, WhitePos, BlackPos).
init(Board, XMid/1, XMid/YMax) :-
    init_window(1920, 1080),
    board_dimensions(XMax, YMax),
    XMid is (1 + XMax) // 2,
    findall(X/Y, (between(1, XMax, X), between(1, YMax, Y)), Board).

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


play(state(Board, WhitePos, BlackPos, white)) :-
    draw_all(Board, WhitePos, BlackPos),
    moves(state(Board, WhitePos, BlackPos, white), Moves), !,
(   repeat,
    user_move(M),
    (
        member(state(Board1, M, BlackPos, black), Moves), !
        ;
        fail
    )
),
    play(state(Board1, M, BlackPos, black)).

play(state(Board, WhitePos, BlackPos, white)) :-
    % No moves left
    draw_all(Board, WhitePos, BlackPos),
    new(@Frame, frame('Black wins')),
    send(@Frame, report, inform, 'Black wins!'),
    % Causes problems - debug why.
    % send(@Frame, report, done),
    % Start another game.
    start.

play(state(Board, WhitePos, BlackPos, black)) :-
    draw_all(Board, WhitePos, BlackPos),
    sleep(0.2),
    % moves(state(Board, WhitePos, BlackPos, black), Moves),
    % bestmove(Moves, BestMove), !,
    search_depth(D),
    minmax(state(Board, WhitePos, BlackPos, black), BestMove, D, _),
    nonvar(BestMove), !,
    play(BestMove).

play(state(Board, WhitePos, BlackPos, black)) :-
    draw_all(Board, WhitePos, BlackPos),
    new(@Frame, frame('White wins')),
    send(@Frame, report, inform, 'White wins!'),
    % Causes problems - debug why.
    %send(@Frame, report, done),
    % Start another game.
    start.


% state(Board, WhitePos, BlackPos, Turn)
% Board - list of tiles of the form X/Y
% WhitePos, BlackPos - tile where white and black players stand,
% respectively
% Turn - an atom which can take the values { white, black }


square_colour(X/Y, brown) :-
    (X + Y) mod 2 =:= 0, !.

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
    GX is X * 100,
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

chr(C, N) :-
    N1 is N + 64, % 64 is uppercase 'A'
    name(C, [N1]).

draw_horizontal_grid([]):-!.
draw_horizontal_grid([X|Tail]):-
    new(@Bo, box(100, 40)),
    GX is X * 100,
    send(@Bo, x(GX)),
    send(@Bo, y(0)),
    send(@Bo, fill_pattern, colour(white)),
    send(@ic, display, @Bo),
    chr(C, X),
    send(@ic, display, new(T, text(C, center, bold))),
    send(T, center, @Bo?center),
    draw_horizontal_grid(Tail).

draw_grid :-
    board_dimensions(XMax, YMax),
    findall(X, between(1, XMax, X), Horizontal),
    findall(Y, between(1, YMax, Y), Vertical),
    draw_vertical_grid(Vertical),
    draw_horizontal_grid(Horizontal).


draw_squares([X/Y|Tail]) :-
    new(@Bo, box(100, 100)),
    GX is X * 100,
    GY is Y * -100,
    send(@Bo, x(GX)),
    send(@Bo, y(GY)),
    square_colour(X/Y, Colour),
    send(@Bo, fill_pattern, colour(Colour)),
    send(@ic, display, @Bo),
    send(@Bo, recogniser, click_gesture(left, '', single, message(@prolog, assert_user_choice, X, Y))),
    draw_squares(Tail).

draw_squares([]).
