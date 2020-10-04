/**
 * Programmers: Assaf Carlsbad and Igor Tsemakhovich
 * File Name:   gui.pl
 * Description: Contains predicates for rendering the game board, handling user input, etc.
 * Other: GUI is based on XPCE. More details can be found here: https://www.swi-prolog.org/packages/xpce/
 */

:- module(gui, [init_window/2, draw_all/3, get_user_choice/1]).
:- dynamic user_choice/1.

% Creates a new window for the game.
init_window(Width, Height) :-
    free(@p), % Destroy the old window in case the game was re-started.
    new(@p, picture('Joust')),
    send(@p, ideal_width(Width)),
    send(@p, ideal_height(Height)),
    send(@p, open).

% Draws the game board, the grids and the two knights.
draw_all(Squares, WhitePos, BlackPos) :-
    % A 'device' is an XPCE object for bundling together several object that can
    % be handled together as one logical unit.
    free(@ic), % Destroy the old device, thus deleting the old board and knights from the game window.
    new(@ic, device),
    draw_squares(Squares),
    draw_grid,
    draw_knight(WhitePos, white),
    draw_knight(BlackPos, black),
    send(@p, display, @ic, point(0, 0)).

% Decides what should be the color for a given square on the board,
% represented as a X/Y pair.
square_colour(X/Y, brown) :-
    (X + Y) mod 2 =:= 0, !.

square_colour(_, yellow).

% Given a square (represented as a X/Y pair) and the color of the
% current player (an atom which can be either white or black), gives the
% full path to the image file for the knight.
knight_pic_file_name(X/Y, white, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "knights/white_brown.jpg", Path),
    square_colour(X/Y, brown).

knight_pic_file_name(X/Y, black, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "knights/black_brown.jpg", Path),
    square_colour(X/Y, brown).

knight_pic_file_name(X/Y, white, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "knights/white_yellow.jpg", Path),
    square_colour(X/Y, yellow).

knight_pic_file_name(X/Y, black, Path):-
    working_directory(CWD, CWD),
    string_concat(CWD, "knights/black_yellow.jpg", Path),
    square_colour(X/Y, yellow).

% Draws a knight of color 'Turn' on the square X/Y.
draw_knight(X/Y, Turn) :-
    GX is X * 80,
    GY is Y * -80,
    knight_pic_file_name(X/Y, Turn, FileName),
    send(@ic, display, new(_, bitmap(FileName)), point(GX, GY)).

% Draws the vertical grid to the left of the board.
draw_vertical_grid([]):-!.
draw_vertical_grid([Y|Tail]):-
    new(@Bo, box(40, 80)),
    GY is Y * -80,
    send(@Bo, x(40)),
    send(@Bo, y(GY)),
    send(@Bo, fill_pattern, colour(white)),
    send(@ic, display, @Bo),
    send(@ic, display, new(T, text(Y, center, bold))),
    send(T, center, @Bo?center),
    draw_vertical_grid(Tail).

% Converts an integer N into its corresponding letter
% according to the order of the alphabet. For example, a value of 4
% for N will yields the letter D.
chr(C, N) :-
    N1 is N + 64, % 64 is uppercase 'A'
    name(C, [N1]).

% Draws the horizontal grid below the board.
draw_horizontal_grid([]):-!.
draw_horizontal_grid([X|Tail]):-
    new(@Bo, box(80, 40)),
    GX is X * 80,
    send(@Bo, x(GX)),
    send(@Bo, y(0)),
    send(@Bo, fill_pattern, colour(white)),
    send(@ic, display, @Bo),
    chr(C, X),
    send(@ic, display, new(T, text(C, center, bold))),
    send(T, center, @Bo?center),
    draw_horizontal_grid(Tail).

% Draws a button used to restart the game prematurely.
draw_restart_button(XMax):-
    new(@Bo, box(160, 80)),
    % Make sure the button is positioned in the center
    GX is (XMax / 2) * 80,
    send(@Bo, x(GX)),
    send(@Bo, y(80)),
    send(@Bo, fill_pattern, colour(gray)),
    send(@ic, display, @Bo),
    send(@ic, display, new(T, text("Restart Game", center, bold))),
    send(T, center, @Bo?center),
    % On clicking the box, call 'joust' again to re-start the game from fresh.
    send(@Bo, recogniser, click_gesture(left, '', single, message(@prolog, joust))).

% Draws the two grids: vertical and horizontal.
draw_grid :-
    board_dimensions(XMax, YMax),
    findall(X, between(1, XMax, X), Horizontal),
    findall(Y, between(1, YMax, Y), Vertical),
    draw_vertical_grid(Vertical),
    draw_horizontal_grid(Horizontal),
    draw_restart_button(XMax).

% Draws the squares that comprise the game board.
draw_squares([X/Y|Tail]) :-
    new(@Bo, box(80, 80)),
    GX is X * 80,
    GY is Y * -80,
    send(@Bo, x(GX)),
    send(@Bo, y(GY)),
    square_colour(X/Y, Colour),
    send(@Bo, fill_pattern, colour(Colour)),
    send(@ic, display, @Bo),
    % Whenever the user clicks on one of the squares, the predicate 'assert_user_choice'
    % will be called with X and Y as arguments.
    send(@Bo, recogniser, click_gesture(left, '', single, message(@prolog, assert_user_choice, X, Y))),
    draw_squares(Tail).

draw_squares([]).

% Called in response to the user clicking on the square X/Y.
% We assert the user choice so that in can be queried later.
assert_user_choice(X, Y) :-
    assertz(user_choice(X/Y)).

% Retrieves the X/Y coordinates of the square the user clicked on.
get_user_choice(Choice) :-
    retract(user_choice(Choice)).
