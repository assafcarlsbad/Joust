:- module(ai, [minmax/4]).

% AI related predicates
staticval(state(Board, WhitePos, BlackPos, _), Val) :-
    % Black is the MAX player.
    (
      moves(state(Board, WhitePos, BlackPos, black), BlackMoves), !,
      length(BlackMoves, BlackLen)
      ;
      BlackLen is -1000
    ),

    (
      moves(state(Board, WhitePos, BlackPos, white), WhiteMoves), !,
      length(WhiteMoves, WhiteLen)
      ;
      WhiteLen is 1000
    ),
    Val is BlackLen - WhiteLen.

minmax(state(Board, WhitePos, BlackPos, Turn), NextPos, Depth, Val) :-
    Depth > 0,
    Depth1 is Depth - 1,
    moves(state(Board, WhitePos, BlackPos, Turn), MovesList), !,
    best(MovesList, NextPos, Depth1, Val)
    ;
    staticval(state(Board, WhitePos, BlackPos, Turn), Val).

% only one candidate
best([Pos], Pos, Depth, Val) :-
    !,
    minmax(Pos, _, Depth, Val).

% several candidates
best([Pos1|PosList], BestPos, Depth, BestVal) :-
    minmax(Pos1, _, Depth, Val1),
    best(PosList, Pos2, Depth, Val2),
    betterof(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterof(Pos1, Val1, Pos2, Val2, Pos1, Val1) :-
    % black is max player
    turn(Pos1, white),
    Val1 >= Val2, !.

betterof(Pos1, Val1, Pos2, Val2, Pos1, Val1) :-
    % white is min player
    turn(Pos1, black),
    Val1 =< Val2, !.

% all other cases
betterof(Pos1, Val1, Pos2, Val2, Pos2, Val2).

