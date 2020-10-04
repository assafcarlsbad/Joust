/**
 * Programmers: Assaf Carlsbad and Igor Tsemakhovich
 * File Name:   ai.pl
 * Description: Contains an implementation for the alpha-beta algorithm,
 *              as well as some auxillary predicates. Implementation is
 *              based on the algorithm as it appears on page 585
 *              in the textbook.
 */

:- module(ai, [alphabeta/6]).

% Black is the MAX player.
% State is of the form: state(Board, WhitePos, BlackPos, Turn).
max_to_move(State) :-
    arg(4, State, black).

% White is the MIN player.
min_to_move(State) :-
    arg(4, State, white).

/**********************
 * Heuristic function *
 **********************/

% White's turn but it has no legal moves - black wins.
staticval(state(Board, WhitePos, BlackPos, white), 1000) :-
    \+moves(state(Board, WhitePos, BlackPos, white), _), !.

% White's turn and black has no legal moves - white wins.
staticval(state(Board, WhitePos, BlackPos, white), -1000) :-
    \+moves(state(Board, WhitePos, BlackPos, black), _), !.

% Black's turn but it has no legal moves - white wins.
staticval(state(Board, WhitePos, BlackPos, black), -1000) :-
    \+moves(state(Board, WhitePos, BlackPos, black), _), !.

% Black's turn and white has no legal moves - black wins.
staticval(state(Board, WhitePos, BlackPos, black), 1000) :-
    \+moves(state(Board, WhitePos, BlackPos, white), _), !.

% Otherwise, the heuristic estimate for a given position is the number
% of legal moves for the black player minus the number of legal moves
% for the white player.
staticval(state(Board, WhitePos, BlackPos, _), Val) :-
    moves(state(Board, WhitePos, BlackPos, black), BlackMoves),
    length(BlackMoves, BlackLen),
    moves(state(Board, WhitePos, BlackPos, white), WhiteMoves),
    length(WhiteMoves, WhiteLen),
    Val is BlackLen - WhiteLen.

alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
    Depth > 0,
    Depth1 is Depth - 1,
    moves(Pos, PosList), !,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, Depth1)
    ;
    % Terminal position or depth exceeded, fall back to the heuristic function.
    staticval(Pos, Val).

/***********************************
 * Auxillary AI related predicates *
 ***********************************/
boundedbest([Pos|PosList], Alpha, Beta, GoodPos, GoodVal, Depth) :-
    % Compute value for the first child position.
    alphabeta(Pos, Alpha, Beta, _, Val, Depth),
    % Compare it against its siblings.
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).

% No siblings, use the value of the first child position.
goodenough([], _, _, Pos, Val, Pos, Val, _) :- !.

% No need to check the siblings of the position.
goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _) :-
    % Value is 'too good' for the MAX player, no reason MIN would choose this path.
    min_to_move(Pos), Val > Beta, !
    ;
    % Value is 'too good' for the MIN player, no reason MAX would choose this path.
    max_to_move(Pos), Val < Alpha, !.

% The position has sibling which cannot be discarded.
goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth) :-
    % Compute new values for Alpha and Beta.
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
    % Recursively compute the best value for the sibling of Pos, yielding Pos1 and Val1.
    boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, Depth),
    % Compare Pos and Pos1, pick the best one.
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
    % MIN values raise the lower end of the search window (Alpha).
    min_to_move(Pos), Val > Alpha, !.

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
    % MAX values lower the upper end of the search window (Beta).
    max_to_move(Pos), Val < Beta, !.

newbounds(Alpha, Beta, _, _, Alpha, Beta). % Don't change the search window.

betterof(Pos, Val, _, Val1, Pos, Val) :-
    % min_to_move succeeds if the parent position of Pos is played by the MAX player.
    min_to_move(Pos),
    % The MAX player strives to maximize the value of his position.
    Val >= Val1, !.

betterof(Pos, Val, _, Val1, Pos, Val) :-
    % max_to_move succeeds if the parent position of Pos is played by the MIN player.
    max_to_move(Pos),
    % The MIN player strives to minimize the value of his position.
    Val =< Val1, !.

% In all other cases Pos1 with value Val1 is better than Pos with value
% Val.
betterof(_, _, Pos1, Val1, Pos1, Val1).

