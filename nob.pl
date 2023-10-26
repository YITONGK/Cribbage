% the logic of 'one_for_his_nob' rule

% one_for_his_nob(+StartCard, +Hand, -Point).
% StartCard has SuiteStart, check whether there is a card(jack, SuiteStart) in Hand
% if yes, Point will be 1, otherwise 0.
one_for_his_nob(card(_, SuiteStart), Hand, Point) :-
    (   member(card(jack, SuiteStart), Hand)
    ->  Point = 1
    ;   Point = 0
    ).
