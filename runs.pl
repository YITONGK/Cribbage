% The logic of 'runs' rule together with some assist predicates.

:- ensure_loaded('utils.pl').

% runs(+Hand, -Points).
% Calculate the total run Points in a Hand
% if there is a 5 run, not check 3 or 4 runs, let Points equals 5.
% if there is any 4 run, not check 3 run further, generate a list of 4, the number of 4s is 
% the number of 4 runs which can be formed from Hand, Points is the sum of 4s.
% if there is no 3 run, Points will be 0, otherwise, generate a list of 3, the number of 3s is
% the number of 3 runs which can be formed from Hand, Points is the sum of 3s.
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
% generating subsets of length Length from Hand and check whether any subset is a run
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
% input an integer list, the predicate will return true if the elements in List are continuous
continuous([_]).
continuous([First, Second|Rst]) :-
    Second is First + 1,
    continuous([Second|Rst]).