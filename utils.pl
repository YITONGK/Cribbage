% Some helper predicates:
% subset/2 and subset/3 are used to generate elements combinations from a list.
% evaluate/2 and order_value/2 are used to turn the rank of a card to an integer.

% subset(+List1, -List2).
subset([], []).
subset([Head|Tail], [Head|SubTail]) :- subset(Tail, SubTail).
subset([_|Tail], SubTail) :- subset(Tail, SubTail).

% subset(+List1, +Length, -List2).
% List2 has a length of Length
subset([], 0, []).
subset([Head|Tail], L, [Head|SubTail]) :-
    L > 0,
    L1 is L - 1,
    subset(Tail, L1, SubTail).
subset([_|Tail], L, SubTail) :-
    subset(Tail, L, SubTail).

% evaluate(+Card, -Value).
% Convert card rank to its corresponding value for 'fifteen_point' summation purpose.
evaluate(card(ace, _), 1).
evaluate(card(2, _), 2).
evaluate(card(3, _), 3).
evaluate(card(4, _), 4).
evaluate(card(5, _), 5).
evaluate(card(6, _), 6).
evaluate(card(7, _), 7).
evaluate(card(8, _), 8).
evaluate(card(9, _), 9).
evaluate(card(10, _), 10).
evaluate(card(jack, _), 10).
evaluate(card(queen, _), 10).
evaluate(card(king, _), 10).

% order_value(+Card, -Value).
% Convert card rank to its corresponding value for 'runs' ordering purpose.
order_value(card(ace, _), 1).
order_value(card(2, _), 2).
order_value(card(3, _), 3).
order_value(card(4, _), 4).
order_value(card(5, _), 5).
order_value(card(6, _), 6).
order_value(card(7, _), 7).
order_value(card(8, _), 8).
order_value(card(9, _), 9).
order_value(card(10, _), 10).
order_value(card(jack, _), 11).
order_value(card(queen, _), 12).
order_value(card(king, _), 13).