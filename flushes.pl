% the logic of 'flushes' rule

:- ensure_loaded('utils.pl').

% flushes(+StartCard, +Hand, -Point).
% if StartCard and the four cards in Hand have the same suites, Point will be 5,
% else if the four cards in Hand have the same suites, Point will be 4, ohterwise 0.
flushes(card(_, SuiteStart), [card(_, S)|RestHand], Point) :-
    (   same_suites(SuiteStart, [card(_, S)|RestHand])
    ->  Point = 5
    ;   same_suites(S, RestHand)
    ->  Point = 4
    ;   Point = 0
    ).

% same_suites(+Suite, +Cards).
% return true if all the cards in Cards have suite Suite.
same_suites(S, [card(_, S)]).
same_suites(S, [card(_, S)|Rst]) :-
    same_suites(S, Rst).