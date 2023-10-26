% Student name: Yitong Kong
% Student ID: 1254947

% Purpose: This file contains the two required predicates which can be used in 
% a card game called Cribbage. 
% hand_value/3 helps to calculate the points a player get according to the cards 
% in his/her hand and the startcard following the five rules.
% select_hand helps the player to determine which four cards to be kept in hand 
% such that the player can maximize the expected value of the hand over all 
% possible start cards.

% hand_value(+Hand, + StartCard, -Value).
% this predicate will receive a list of 4 cards which is Hand and a StartCard as 
% input, using the five rules to calculate the total point and assign to Value.
hand_value(Hand, Startcard, Value) :-
    % the 5 predicates below will take the StartCard and the 4 cards in Hand as
    % argument, then output the corresponding points the player gets
    fifteen_point([Startcard|Hand], P1),
    pairs([Startcard|Hand], P2),
    runs([Startcard|Hand], P3),
    flushes(Startcard, Hand, P4),
    one_for_his_nob(Startcard, Hand, P5),
    Value is (P1 + P2 + P3 + P4 + P5).

% select_hand(+Cards, -Hand, -Cribcards).
% Give a list of 5 or 6 cards, this predicate will use its algorithm to 
% determine what cards to keep in Hand, and what cards to throw into Cribcards 
% to maximize the benefit    
select_hand(Cards, Hand, Cribcards) :-
    % generate all four-card combinations from the given Cards before evaluation
    findall(PossibleHand, subset(Cards, 4, PossibleHand), PossibleHands),
    % find the Hand of the highest expedcted value.
    find_best_hand(PossibleHands, Cards, [], 0, Hand),
    % Cribcards can be calculated by deleting elements in Hand from Cards.
    subtract(Cards, Hand, Cribcards).

% find_best_hand(+PossibleHands, +Cards, +CurrentBestHand, 
%                +CurrentBestValue, -Hand).
% for a PossibleHand in PossibleHands, if the expected value is greater than the
% CurrentBestValue, then we will update the CurrentBestHand with this 
% PossibleHand, and going on with the next PossibleHand, if not greater than the 
% CurrentBestValue, make no changes to CurrentBestHand and just go on with the 
% next PossibleHand. Finally we will let Hand equals CurrentBestHand.
find_best_hand([], _, Hand, _, Hand).
find_best_hand([PossibleHand|Rest], Cards, CurrentBestHand, 
               CurrentBestValue, Hand) :-
    expected_value(PossibleHand, Cards, Value),
    ( Value > CurrentBestValue
    ->  find_best_hand(Rest, Cards, PossibleHand, Value, Hand)
    ;   find_best_hand(Rest, Cards, CurrentBestHand, CurrentBestValue, Hand)
    ).

% expected_value(+Hand, +VisibleCards, -ExpectedValue).
% use subtract/3 to remove the VisibleCards from the 52 cards list and get a 
% list containing PossibleStartCards. Then calculate the value of Hand with 
% every PossibleStartCard, sum them up and divide by the number of 
% PossibleStartCards, we get ExpectedValue.
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
    findall(Value, (member(StartCard, PossibleStartCards), 
            hand_value(Hand, StartCard, Value)), Values),
    sum_list(Values, SumValues),
    length(PossibleStartCards, L),
    ExpectedValue is SumValues / L.

% fifteen_point(+Hand, -Point).
% 2 points are scored for each distinct combination of cards that add to 15. 
% For this purpose, an ace is counted as 1, and a jack, queen or king are 
% counted as 10, and other cards count as their face value.
fifteen_point(Hand, Point) :-
    findall(Combination, (subset(Hand, Combination), sum_cards(Combination, 15)), 
            Combinations),
    length(Combinations, N),
    Point is N * 2.

% sum_cards(+Cards, -Total)
% this predicate will succeed if the value of card in Cards sum to Total
sum_cards([], 0).
sum_cards([Card | Rest], Total) :-
    evaluate(Card, Value),
    sum_cards(Rest, Subtotal),
    Total is Value + Subtotal.

% pairs(+Hand, -Point). 
% 2 points are scored for each possible pair(two cards with same rank).
pairs(Hand, Point) :-
    findall(Pair, (subset(Hand, Pair), Pair = [card(R1, _), card(R2, _)], 
            R1 == R2), AllPairs),
    length(AllPairs, N),
    Point is N * 2.

% runs(+Hand, -Points).
% 1 point is scored for each card in a distinct combination of 3 or more 
% consecutive cards.
runs(Hand, Points) :-
    (   run_points(Hand, 5)     
    ->  Points = 5
    ;   run_points(Hand, 4)     
    ->  findall(4, run_points(Hand, 4), PointsList),
        sum_list(PointsList, Points)        
    ;   run_points(Hand, 3)
    ->  findall(3, run_points(Hand, 3), PointsList),
        sum_list(PointsList, Points)
    ;   Points = 0
    ).

% run_points(+Hand, +Length).
% this predicate will succeed if a run of length Length can be found in Hand by
% generating subsets of length Length from Hand and check whether any is a run
run_points(Hand, Length) :-
    subset(Hand, Length, Run),
    is_run(Run).

% is_run(+Cards).
% this predicate will succeed if the input card list Card can form a run.
is_run(Cards) :-
    maplist(order_value, Cards, Values),
    msort(Values, SortedValues),
    continuous(SortedValues).
    
% continuous(+Lists)
% input an integer list, return true if the elements in List are continuous.
continuous([_]).
continuous([First, Second|Rst]) :-
    Second is First + 1,
    continuous([Second|Rst]).

% flushes(+StartCard, +Hand, -Point).
% 4 points is scored if all the cards in the hand are of the same suit. 1 
% further point is scored if the start card is also the same suit.
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

% one_for_his_nob(+StartCard, +Hand, -Point).
% 1 point is scored if the hand contains the jack of the same suit as the start 
% card.
one_for_his_nob(card(_, SuiteStart), Hand, Point) :-
    (   member(card(jack, SuiteStart), Hand)
    ->  Point = 1
    ;   Point = 0
    ).

% below are some helper predicates:
% subset/2 and subset/3 are used to generate elements combinations from a list.
% evaluate/2 and order_value/2 are to turn the rank of a card to an integer.

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
% Convert card rank to its corresponding value for '15-point' summation purpose.
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