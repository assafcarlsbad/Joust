/**
 * Programmers: Assaf Carlsbad and Igor Tsemakhovich
 * File Name:   joust.pl
 * Description: Driver code for the game.
 * Input:       config file found in the same directory as this file.
 * Synopsys:    run start.bat file, game should start automatically. If
 *              for some reason it doesn't, consult this file and
 *              execute the 'joust.' predicate.
 */


:- use_module(library(pce)).
:- use_module(moves).
:- use_module(ai).
:- use_module(gui).

:- dynamic user_choice/1.

% Starts the game automatically.
:- initialization(joust).

% Maps a difficulty level to a search depth.
search_depth(1) :-
    difficulty(novice), !.
search_depth(2) :-
    difficulty(easy), !.
search_depth(3) :-
    difficulty(medium), !.
search_depth(4) :-
    difficulty(hard), !.
search_depth(5) :-
    difficulty(ultra), !.
search_depth(6) :-
    difficulty(demigod), !.

% Predicates for reading and processing the config file.
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

% Main game predicate. Reads the configuration, sets the board to its
% initial state and kicks off the game.
joust :-
    read_game_config,
    init(Board, WhitePos, BlackPos),
    % Each state of the game is represented by the term state(Board, WhitePos, BlackPos, Turn), where:
    % Board is a list of squares, each in the form of X/Y.
    % WhitePos is the square where the white knight stands.
    % BlackPos is the square where the black knight stands.
    % Turn is an atom which can take one of two possible values: black or white.
    play(state(Board, WhitePos, BlackPos, white)).


% Generates the starting position of the game.
% The two knights should stand at the middle of the two extremist rows.
init(Board, XMid/1, XMid/YMax) :-
    init_window(1920, 1080),
    board_dimensions(XMax, YMax),
    XMid is (1 + XMax) // 2,
    findall(X/Y, (between(1, XMax, X), between(1, YMax, Y)), Board).

% Retrieves the square the user wants to move into.
user_move(M) :-
    % Poll until a user choice is made.
    repeat,
    (
        get_user_choice(M), !
        ;
        % To avoid wasting too much CPU cycles,
        % we sleep for 0.2 seconds between two consequtive polls.
        sleep(0.2),
        fail
    ).

% White (human) player turn.
play(state(Board, WhitePos, BlackPos, white)) :-
    draw_all(Board, WhitePos, BlackPos),
    % Generate a list of all possible moves for the white knight.
    moves(state(Board, WhitePos, BlackPos, white), Moves), !,
    (
        % Check that the white knight can legally move to the square chosen by the user.
        repeat,
        user_move(M),
        (
            member(state(Board1, M, BlackPos, black), Moves), !
            ;
            fail
        )
    ),
    % Continue from the new state, black's turn.
    play(state(Board1, M, BlackPos, black)).

play(state(Board, WhitePos, BlackPos, white)) :-
    % No legal moves are available for the white, thus black wins.
    draw_all(Board, WhitePos, BlackPos),
    new(@Frame, frame('Black wins')),
    send(@Frame, report, inform, 'Black wins!'),
    % Re-start the game.
    joust.

% Black (AI) player turn.
play(state(Board, WhitePos, BlackPos, black)) :-
    draw_all(Board, WhitePos, BlackPos),
    search_depth(D),
    % Search for the best possible move, after evaluating the game tree D levels.
    alphabeta(state(Board, WhitePos, BlackPos, black), -1000, 1000, BestMove, _, D),
    % When we reach a terminal position in the game tree 'alphabeta' does not instantiate BestMove.
    % Therefore, nonvar will succeed if we have at least one possible move.
    nonvar(BestMove), !,
    % Continue to play after making the move, white's turn.
    play(BestMove).

play(state(Board, WhitePos, BlackPos, black)) :-
    % We get here only if 'nonvar(BestMove)' failed, meaning we have no moves left - white wins.
    draw_all(Board, WhitePos, BlackPos),
    new(@Frame, frame('White wins')),
    send(@Frame, report, inform, 'White wins!'),
    % Re-start the game.
    joust.
