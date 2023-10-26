% The logic of 'pairs' rule.

% pairs(+Hand, -Point). 
% check for pairs(two cards with same rank), Point is 2 times the total number of pairs.
pairs(Hand, Point) :-
    findall(Pair, (subset(Hand, Pair), Pair = [card(R1, _), card(R2, _)], R1 == R2), AllPairs),
    length(AllPairs, N),
    Point is N * 2.