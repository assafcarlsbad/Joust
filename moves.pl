:- module(moves, [moves/2, turn/2]).

turn(State, Turn) :-
    % A game state is represented as a 4-member object:
    % state(Board, WhitePos, BlackPos, Turn).
    arg(4, State, Turn).

knight_move(X/Y, X1/Y1) :-
    % Move North-East.
    X1 is X + 1,
    Y1 is Y + 2.

knight_move(X/Y, X1/Y1) :-
    % Move East-North.
    X1 is X + 2,
    Y1 is Y + 1.

knight_move(X/Y, X1/Y1) :-
    % Move East-South.
    X1 is X + 2,
    Y1 is Y - 1.

knight_move(X/Y, X1/Y1) :-
    % Move South-East.
    X1 is X + 1,
    Y1 is Y - 2.

knight_move(X/Y, X1/Y1) :-
    % Move North-West.
    X1 is X - 1,
    Y1 is Y - 2.

knight_move(X/Y, X1/Y1) :-
    % Move West-South.
    X1 is X - 2,
    Y1 is Y - 1.

knight_move(X/Y, X1/Y1) :-
    % Move West-North.
    X1 is X - 2,
    Y1 is Y + 1.

knight_move(X/Y, X1/Y1) :-
    % Move North-West.
    X1 is X - 1,
    Y1 is Y + 2.

move(state(Board, WhitePos, BlackPos, white), state(Board1, WhitePos1, BlackPos, black)) :-
    % White moves.
    delete(Board, WhitePos, Board1),
    knight_move(WhitePos, WhitePos1),
    member(WhitePos1, Board1).

move(state(Board, WhitePos, BlackPos, black), state(Board1, WhitePos, BlackPos1, white)) :-
    % Black moves.
    delete(Board, BlackPos, Board1),
    knight_move(BlackPos, BlackPos1),
    member(BlackPos1, Board1).

% Generates a list of all legal moves from a given state.
% Fails if no moves are legal.
moves(State, [M1|Ms]) :-
    findall(State1, move(State, State1), [M1|Ms]).
