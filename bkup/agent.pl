% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Assignment 3 for 9414 Semester 1, 2017
%
% Team: Keshi Chen, 5142821
%       Zhengting Li,
%
% Assignment name: Project 3, Option 2: Prolog (BDI Agent)
%
% In this assignment we will write a simple BDI Agent with basic
% functions that operates in a Gridworld.
%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 1]
% A predicate that takes a list of events, each with the form
% truffle(X,Y,S) or restaurant(X,Y,S), then computes the corresponding
% list of goals for the agent, each with the form goal(X,Y,S).
%
% trigger(+Events, -Goals).
%   Takes a list of Events, each with the form truffle(X,Y,S) or
%   restaurant(X,Y,S), and computes the corresponding list of Goals for
%   the agent, each of the form goal(X,Y,S).
%   % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

trigger([], goals([],[])).
trigger([truffle(X, Y, S)|Tail], goals(Goals_rest,[goal(X,Y,S)|Goals_truff])) :-
    trigger(Tail, goals(Goals_rest, Goals_truff)).
trigger([restaurant(X, Y, S)|Tail], goals([goal(X,Y,S)|Goals_rest],Goals_truff)) :-
    trigger(Tail, goals(Goals_rest, Goals_truff)).



% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 2]
% A predicate which has four arguments:
% - a tuple contains two lists of goals for restaurant and
% truffle respectively.
% list of goals each of the form goal(X,Y,S),
% - beliefs, with the form beliefs(at(X,Y),stock(T)),
% - a tuple contains two lists of the current intentions, each with
% the form [[goal(X,Y,S), Plan] | Tail],
% - a tuple contains two lists to be computed which contains the new goals inserted into the
% current list of intentions in decreasing order of value, using the distance from the agent to target.
% A new goal will be placed immediately before the first goal in the
% list that has a lower value or which has an equal value and is further
% away from the agent's current position, without reordering the current
% list of goals.
%
% incorporate_goals(+Goals, +Beliefs, +Intentions, -Intentions1).
%   Takes Goals list and inserts only the new goals into the Intentions list
%   immediately before an intention with a goal of a lower value. By lower value
%   first, the Score is compared with the Manhattan distance if the scores are
%   the same. The plan associated with each newly inserted goal is the
%   empty plan.
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% Base case, if no more Goals then we just copy the old intentions to
% new one.
incorporate_goals(goals([],[]), _, intents(Int_sell,Int_pick), intents(Int_sell,Int_pick)).

% For restaurant goals:
% The current goal is already in the intentions list so we ignore it.
incorporate_goals(goals([Goals_rest|Gr_Tail],Goals_truff), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)) :-
    is_member(Goals_rest, Int_sell),
    incorporate_goals(goals(Gr_Tail,Goals_truff), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)).

% Only when it is not in the intentions list will we insert.
incorporate_goals(goals([Goals_rest|Gr_Tail],Goals_truff), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)) :-
    not(is_member(Goals_rest, Int_sell)),
    insert_goal(Goals_rest,Int_sell, Beliefs, UP_sell),
    incorporate_goals(goals(Gr_Tail,Goals_truff), Beliefs, intents(UP_sell,Int_pick), intents(Int_sell1,Int_pick1)).

% For truffle goals:
% The current goal is already in the intentions list so we ignore it.
incorporate_goals(goals(Goals_rest,[Goals_truff|Gt_Tail]), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)) :-
    is_member(Goals_truff, Int_pick),
    incorporate_goals(goals(Goals_rest,Gt_Tail), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)).
% Only when it is not in the intentions list will we insert.
incorporate_goals(goals(Goals_rest,[Goals_truff|Gt_Tail]), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)) :-
    not(is_member(Goals_truff, Int_pick)),
    insert_goal(Goals_truff, Int_pick, Beliefs, UP_pick),
    incorporate_goals(goals(Goals_rest,Gt_Tail), Beliefs, intents(Int_sell,UP_pick), intents(Int_sell1,Int_pick1)).




% insert_goal(+Goal, +Intentions, +Belief, -Intentions1).
%   Insert the Goal into the Intentions, following descend order.
insert_goal(Goal,[],_,[[Goal,[]]]).
insert_goal(Goal, [Intent|Intentions], Belief, [Intent|Intentions1]):-
    not(gtp(Goal, Intent, Belief)), !,
    insert_goal(Goal, Intentions, Belief, Intentions1).

insert_goal(X, Intentions, _, [[X, []]|Intentions]).

% is_member(+Goal, +Intentions).
%   Check if a goal is in the intentions list. Each item in the
%   intentions list is a two member list of the format [Goal, Plan]. The
%   Plan contains a list of actions.

is_member(Goal, [Head|_]) :-
    member(Goal, Head).

is_member(Goal, [Head|Tail]) :-
    not(member(Goal, Head)),
	is_member(Goal, Tail).

% gtp(+Goal, +Plan, -Belief).
%   Compare the Goal's Score (the 3rd parameter of Goal) with the Score
%   of the goal in the head of the Plan list and is greater if the Score
%   of Goal is greater, or if values of Score are equal, the one with
%   the shortest distance to the Belief. Notice that the greater-than
%   signs have been reversed as we want the list in decending order.

% Compare scores.
gtp(goal(_, _, S1), [goal(_, _, S2)|_], _) :-
    S1 > S2.

% Compare distances to Belief.
gtp(goal(X1, Y1, S1), [goal(X2, Y2, S2)|_], [beliefs(at(X, Y),stock(_))|_]) :-
    S1 == S2,
    distance((X, Y), (X1, Y1), D1),
    distance((X, Y), (X2, Y2), D2),
    D1 < D2.


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 3]
% A predicate get_action which takes the agent's beliefs and the list of
% intentions as parameters, and gives an action to be taken by the
% agent and the updated list of intentions.
%
% select_action(+Beliefs, +Intentions, -Intentions, -Action).
%   Selects the next action for the agent to perform from the list of
%   Intentions. If there are none, then it stays at where it is.
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% If the intentions are empty then stay still.
get_action(beliefs(at(X, Y),stock(_)), intents([],[]), intents([],[]), move(X, Y)).

% If the Action is good, use it and update the Intentions list ...
%
% get_action(beliefs(at(X1,Y1),_), intents([],[[Goal,[]]|Int_pick]), intents([],[[Goal, []]|Int_pick]), Action) :-
%    Action = move(X1,Y1),
%    write('TEST7: ACTION:'),writeln(Action).

get_action(_, intents([[goal(X,Y,S),Plan]|Int_sell],Int_pick), intents([[Goal, NextActions]|Int_sell],Int_pick), Action) :-
    decompose_intention([goal(X,Y,S),Plan], Goal, [Action|NextActions]),
    applicable(Action),
    write('TEST1: ACTION:'),writeln(Action).

% get_action(Beliefs, intents([[goal(X,Y,S),Plan]|Int_sell],Int_pick), intents([[goal(X,Y,S), Plan]|Int_sell],Int_pick), Action) :-
%    new_plan_s(goal(X,Y,S), Beliefs, NewPlan),
%    next_action(NewPlan, Plan, Action ),
%    write('TEST2: ACTION:'),writeln(Action).

get_action(beliefs(At,stock(T)), intents([[goal(X,Y,S),Plan]|Int_sell],Int_pick), intents([[Goal, NextActions]|Int_sell],Int_pick), Action) :-
    decompose_intention([goal(X,Y,S),Plan], Goal, [FailAction|_]),
    not(applicable(FailAction)),
    new_plan_s(Goal, beliefs(At,stock(T)), NewPlan),
    next_action(NewPlan, NextActions, Action),
    write('TEST3: ACTION:'),writeln(Action).

get_action(_, intents(Int_sell,[[goal(X,Y,S),Plan]|Int_pick]), intents(Int_sell,[[Goal, NextActions]|Int_pick]), Action) :-
    decompose_intention([goal(X,Y,S),Plan], Goal, [Action|NextActions]),
    applicable(Action),
    write('TEST4: ACTION:'),writeln(Action).

% get_action(Beliefs, intents(Int_sell,[[goal(X,Y,S),[]]|PTail]), intents(Int_sell,[[goal(X,Y,S), Plan]|PTail]), Action) :-
%     new_plan_p(goal(X,Y,S), Beliefs, NewPlan), next_action(NewPlan,
% Plan, Action ), write('TEST5: ACTION:'),writeln(Action).

get_action(Beliefs, intents([],[[goal(X,Y,S),Plan]|Int_pick]), intents([],[[Goal, NextActions]|Int_pick]), Action) :-
    decompose_intention([goal(X,Y,S),Plan], Goal, [FailAction|_]),
    not(applicable(FailAction)),
    new_plan_p(Goal, Beliefs, NewPlan),
    next_action(NewPlan, NextActions, Action),
    write('TEST6: ACTION:'),writeln(Action).


% ... otherwise Action is not applicable so create a new Plan for the Goal.
%get_action(Beliefs, [Intent|Tail], [[Goal, Plan]|Tail], Action) :-
%    decompose_intention(Intent, Goal, [BadAction|_]),
%    not(applicable(BadAction)),
%    new_plan(Goal, Beliefs, NewPlan),
%    next_action(NewPlan, Plan, Action).

% next_action(+PrevPlan, -Plan, -Action).
%   Pops out the first Action from PrevPlan and returns the Plan
%   without it and the first Action of it.
next_action([Action|Plan], Plan, Action).

% decompose_intention(+Intention, -Goal, -Plan).
%   Extract Goal and Plan from Intention.
decompose_intention([Goal,Plan], Goal, Plan).

% new_plan(+Goal, +Beliefs, -Plan).
%   Generate a list of move() actions ending with a pick() or a
%   sell() action based on the agent's current at().
%   '_s' means that end with sell(), '_p' means that end with pick().
new_plan_s(Goal, Beliefs, Plan) :-
    new_plan_s(Goal, Beliefs, [], Plan).

new_plan_s(goal(X, Y, _), beliefs(at(X, Y),stock(_)), PartialPlan, Plan) :-
    reverse([sell(X, Y)|PartialPlan], Plan).

new_plan_s(Goal, beliefs(at(X, Y),stock(T)), PartialPlan, Plan) :-
    valid_move(X, Y, move(Xnew, Ynew)),
    h(move(Xnew, Ynew), Goal, at(X, Y)),
    new_plan_s(Goal, beliefs(at(Xnew, Ynew),stock(T)), [move(Xnew, Ynew)|PartialPlan], Plan).

new_plan_p(Goal, Beliefs, Plan) :-
    new_plan_p(Goal, Beliefs, [], Plan).

new_plan_p(goal(X, Y, _), beliefs(at(X, Y),stock(_)), PartialPlan, Plan) :-
    reverse([pick(X, Y)|PartialPlan], Plan).


new_plan_p(Goal, beliefs(at(X, Y),stock(T)), PartialPlan, Plan) :-
    valid_move(X, Y, move(Xnew, Ynew)),
    h(move(Xnew, Ynew), Goal, at(X, Y)),
    new_plan_p(Goal, beliefs(at(Xnew, Ynew),stock(T)), [move(Xnew, Ynew)|PartialPlan], Plan).

% h(+Move, +Goal, +Belief).
%   The heuristic function to determine whether a Move is in the correct
%   direction, using Manhattan distance. Move has to
%   be closer to the Goal than agent's current position.

h(move(X, Y), goal(Xg, Yg, _), at(Xr, Yr)) :-
    distance((X, Y), (Xg, Yg), Dm),
    distance((Xr, Yr), (Xg, Yg), Dr),
    Dm < Dr.

% valid_move(+X, +Y, -Move).
%   Find out all valid moves for a given coordinate.

valid_move(X, Y, Move) :-
    Dx is X + 1, Move = move(Dx, Y);
    Dx is X - 1, Move = move(Dx, Y);
    Dy is Y + 1, Move = move(X, Dy);
    Dy is Y - 1, Move = move(X, Dy).

% reverse(+List, -Reverse).
% reverse(+List, ?PartReversed, -Reversed).
%   Reversed is obtained by adding the elements of List in reverse order
%   to PartReversed. (See Bratko, 3rd Ed. p.188)

reverse(List, Reversed) :-
    reverse(List, [], Reversed).

reverse([], Reversed, Reversed).

reverse([X|Rest], PartReversed, TotalReversed) :-
    reverse(Rest, [X|PartReversed], TotalReversed).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 4]
% Two predicates, update_beliefs and update_intentions that will compute the
% lists of beliefs and intentions resulting from the agent's observations.
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% update_beliefs(+Observation, @Beliefs, -Beliefs1).
%   Update robots Beliefs based on Observations. Replace the old at()
%   with the new at().

update_beliefs(at(X, Y), beliefs(at(_,_),stock(T)), beliefs(at(X,Y),stock(T))).
update_beliefs(picked(X, Y, S), beliefs(at(X,Y),stock(T)), beliefs(at(X,Y),stock(T1))):-
    T1 is S + T.
update_beliefs(sold(X, Y, S), beliefs(at(X,Y),stock(T)), beliefs(at(X,Y),stock(T1))):-
    T1 is T - S.

% ignore cleaned() observations.
%update_beliefs(_, Beliefs, Beliefs).


% update_intentions(+Observation, +Intentions, -Intentions1).
%   Update intentions based on Observations. Remove the goal once the junk has
%   been cleaned. Assuming its still the last goal to have been reached.

update_intentions(picked(X, Y, S), intents(Int_sell,[[goal(X, Y, S),_]|Intentions1]), intents(Int_sell,Intentions1)).
update_intentions(sold(X, Y, S), intents([[goal(X, Y, S),_]|Intentions1],Int_pick), intents(Intentions1,Int_pick)).
update_intentions(at(_, _), Intentions, Intentions).
% Catch the rest to stop backtracking.
%update_intentions(_, Intentions, Intentions).
