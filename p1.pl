% Student name: Yitong Kong
% Student ID: 1254947

% This file is the main file which contains the two required predicates
% hand_value/3, and select_hand/3.

:- ensure_loaded('fifteen_points.pl').
:- ensure_loaded('pairs.pl').
:- ensure_loaded('runs.pl').
:- ensure_loaded('flushes.pl').
:- ensure_loaded('nob.pl').

% hand_value(+Hand, + StartCard, -Value).
% this predicate will receive a list of 4 cards which is Hand and a StartCard as input,
% using the five rules we have implemented and calculate the total point and assign to Value.
hand_value(Hand, Startcard, Value) :-
    fifteen_point([Startcard|Hand], P1),
    pairs([Startcard|Hand], P2),
    runs([Startcard|Hand], P3),
    flushes(Startcard, Hand, P4),
    one_for_his_nob(Startcard, Hand, P5),
    Value is (P1 + P2 + P3 + P4 + P5).

% select_hand(+Cards, -Hand, -Cribcards).
% Give a list of 5 or 6 cards, this predicate will use its algorithm to determine 
% what cards to keep in Hand, and what cards to throw into Cribcards to maximize the benefit    
select_hand(Cards, Hand, Cribcards) :-
    % generate all the four-card combinations from the given Cards before evaluation.
    findall(PossibleHand, subset(Cards, 4, PossibleHand), PossibleHands),
    % find the Hand of the highest expedcted value.
    find_best_hand(PossibleHands, Cards, [], 0, Hand),
    % Cribcards can be calculated by deleting elements in Hand from Cards.
    subtract(Cards, Hand, Cribcards).

% find_best_hand(+PossibleHands, +Cards, +CurrentBestHand, +CurrentBestValue, -Hand).
% for a PossibleHand in PossibleHands, if the expected value is greater than the CurrentBestValue,
% then we will update the CurrentBestHand with this PossibleHand, and going on with the next
% PossibleHand, if not greater than the CurrentBestValue, make no changes to CurrentBestHand 
% and just go on with the next PossibleHand. Finally we will let Hand equals CurrentBestHand.
find_best_hand([], _, Hand, _, Hand).
find_best_hand([PossibleHand|Rest], Cards, CurrentBestHand, CurrentBestValue, Hand) :-
    expected_value(PossibleHand, Cards, Value),
    ( Value > CurrentBestValue
    ->  find_best_hand(Rest, Cards, PossibleHand, Value, Hand)
    ;   find_best_hand(Rest, Cards, CurrentBestHand, CurrentBestValue, Hand)
    ).

% expected_value(+Hand, +VisibleCards, -ExpectedValue).
% use subtract/3 to remove the VisibleCards from the 52 cards list and get a list containing 
% PossibleStartCards. Then calculate the value of Hand with every PossibleStartCard, sum them
% up and divide by the number of PossibleStartCards, we get ExpectedValue.
expected_value(Hand, VisibleCards, ExpectedValue) :-
    subtract([card(ace, hearts), card(ace, diamonds), card(ace, clubs), card(ace, spades),
            card(2, hearts), card(2, diamonds), card(2, clubs), card(2, spades),
            card(3, hearts), card(3, diamonds), card(3, clubs), card(3, spades),
            card(4, hearts), card(4, diamonds), card(4, clubs), card(4, spades),
            card(5, hearts), card(5, diamonds), card(5, clubs), card(5, spades),
            card(6, hearts), card(6, diamonds), card(6, clubs), card(6, spades),
            card(7, hearts), card(7, diamonds), card(7, clubs), card(7, spades),
            card(8, hearts), card(8, diamonds), card(8, clubs), card(8, spades),
            card(9, hearts), card(9, diamonds), card(9, clubs), card(9, spades),
            card(10, hearts), card(10, diamonds), card(10, clubs), card(10, spades),
            card(jack, hearts), card(jack, diamonds), card(jack, clubs), card(jack, spades),
            card(queen, hearts), card(queen, diamonds), card(queen, clubs), card(queen, spades),
            card(king, hearts), card(king, diamonds), card(king, clubs), card(king, spades)],
            VisibleCards, PossibleStartCards),
    findall(Value, (member(StartCard, PossibleStartCards), hand_value(Hand, StartCard, Value)), Values),
    sum_list(Values, SumValues),
    length(PossibleStartCards, L),
    ExpectedValue is SumValues / L.











