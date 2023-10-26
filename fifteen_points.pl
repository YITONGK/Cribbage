% The logic of 'fifteen_point' rule.

:- ensure_loaded('utils.pl').

% fifteen_point(+Hand, -Point).
% Given a list of card, count the number of subsets of Hand that sum to 15, times 2 is the Point.
fifteen_point(Hand, Point) :-
    findall(Combination, (subset(Hand, Combination), sum_cards(Combination, 15)), Combinations),
    length(Combinations, N),
    Point is N * 2.

% sum_cards(+Cards, -Total)
% this predicate will succeed if the value of card in Cards sum to Total
sum_cards([], 0).
sum_cards([Card | Rest], Total) :-
    evaluate(Card, Value),
    sum_cards(Rest, Subtotal),
    Total is Value + Subtotal.