
% Set the pathways to represent the map
% (Use underscore to represent a space)
connected(fanged_forest,dragons_peak).
connected(fanged_forest,lowlands).
connected(dragons_peak,fanged_forest).
connected(dragons_peak,wailing_cliffs).
connected(wailing_cliffs,lowlands).
connected(lowlands,wailing_cliffs).
connected(lowlands,lost_lake).
connected(lowlands,fanged_forest).
connected(lowlands,home).
connected(home,lowlands).

% Get paths between X and Y


% from Moodle
rev([],[]).
rev([H|T],R) :- rev(T,R1), append(R1,[H],R).

% Several transform rules that transform abstract
% actions into appropriate other actions

% Base case
transform([],[],_).

% Move to L from a directly connected location
transform(move(A,L), [travel(A,D,L)], S) :-
  \+ member(at(A,L),S),
  connected(D,L),
  member(at(A,D),S).

% Move to L from a non-directly connected location
transform(move(A,L), [travel(A,D,H)|T], S) :-
  member(at(A,D), S),
  \+ member(at(A,L), S),
  \+ connected(D,L),
  path(L,D,P),
  rev(P,[_,H|_]),
  select(at(A,D), S, S0),
  transform(move(A,L), T, [at(A,H)|S0]).

% Pick up an object
transform(get(A,Obj), Ac, S) :-
  \+ member(has(A,Obj), S),
  member(at(Obj,ObjLoc), S),
  \+ member(at(A,ObjLoc), S),
  transform(move(A,ObjLoc), Ac0, S),
  append(Ac0, [take(A,Obj)], Ac).

% Do rules for updating the state after abstract actions
% are performed (get,move).

% Update state after a move
do(move(A,L), S, S1) :-
  member(at(A,D), S),
  select(at(A,D), S, S0),
  append([at(A,L)], S0, S1).

% Update state after picking up an object
do(get(A,Obj), S, S2) :-
  member(at(A,L),S),
  member(at(Obj,ObjLoc), S),
  select(at(A,L), S, S0),
  select(at(Obj,ObjLoc), S0, S1),
  append([at(A,ObjLoc), has(A,Obj)], S1, S2),
  !.

% Recursive story rule that forms the story

% Base case
getStory([],_,_,_).

% Only one goal
getStory([G],S,A,W) :-
  transform(G,S,A),
  do(G,A,W),
  !.

% Loop through many goals
getStory([G|T],S,A,W2) :-
  transform(G,S1,A),
  do(G,A,W1), !,
  append(S1,S2,S),
  getStory(T,S2,W1,W2).

% Text output for each specific action/state

% Create better presentation of locations for the text output
% (eg: fanged_forest => FANGED FOREST)

makePretty(S,P) :-
    split_string(S,"_","",L),
    atomic_list_concat(L,' ',A),
    upcase_atom(A,P).

% Base case
textOp([]).

% Recursive call
textOp([H|T]) :- textOp(H), textOp(T).

% Text output for each action/state

textOp(at(A,L)) :-
  write(A),
  write(' is at '),
  makePretty(L,PL),
  write(PL),
  nl.

textOp(has(A,Obj)) :-
  write(A),
  write(' has the '),
  write(Obj),
  nl.

textOp(take(A,Obj)) :-
  write(A),
  write(' takes '),
  write(Obj),
  nl.

textOp(travel(A,D,L)) :-
  write(A),
  write(' moves from '),
  makePretty(D,PD),
  write(PD),
  write(' to '),
  makePretty(L,PL),
  write(PL),
  nl.

% Main story rule, tells the story as text output

% This will give output for S and W
story(G,S,A,W) :-
  getStory(G,S,A,W),
  !.

% This just tells the story
story(G,A) :-
  write('Welcome to Adventure Island!'),
  nl,
  textOp(A),
  getStory(G,S,A,W),
  textOp(S),
  textOp(W),
  !.

run_hero :-
  story([get(hero, treasure), move(hero, home)],
    [at(hero, home), at(treasure, dragons_peak)]).

run2 :-
  story([get(ved, sword), move(ved, dragons_peak)], 
  [at(ved, home), at(sword, fanged_forest)]).

  % Set the pathways to represent the map
% (Use underscore to represent a space)
connected(kitchen, corridor).
connected(corridor, ballroom).
connected(corridor, garden).
connected(ballroom, corridor).
connected(garden, corridor).
connected(garden, forest).
connected(forest, garden).
connected(forest, castle).

% Find paths between locations
path(X, Y, P) :- path(X, Y, [], P).
path(X, X, _, []).
path(X, Y, Visited, [Next|P]) :-
    connected(X, Next),
    \+ member(Next, Visited),
    path(Next, Y, [Next|Visited], P).

% Reverse a list
rev([],[]).
rev([H|T],R) :- rev(T,R1), append(R1,[H],R).

% Transform abstract actions into appropriate other actions

% Base case
transform([],[],_).

% Move to Location from a directly connected location
transform(move(A,L), [travel(A,D,L)], S) :-
    \+ member(at(A,L),S),
    connected(D,L),
    member(at(A,D),S).

% Move to Location from a non-directly connected location
transform(move(A,L), [travel(A,D,H)|T], S) :-
    member(at(A,D), S),
    \+ member(at(A,L), S),
    \+ connected(D,L),
    path(L,D,P),
    rev(P,[_,H|_]),
    select(at(A,D), S, S0),
    transform(move(A,L), T, [at(A,H)|S0]).

% Pick up an object
transform(get(A,Obj), Ac, S) :-
    \+ member(has(A,Obj), S),
    member(at(Obj,ObjLoc), S),
    \+ member(at(A,ObjLoc), S),
    transform(move(A,ObjLoc), Ac0, S),
    append(Ac0, [take(A,Obj)], Ac).

% Update state after a move
do(move(A,L), S, S1) :-
    member(at(A,D), S),
    select(at(A,D), S, S0),
    append([at(A,L)], S0, S1).

% Update state after picking up an object
do(get(A,Obj), S, S2) :-
    member(at(A,L),S),
    member(at(Obj,ObjLoc), S),
    select(at(A,L), S, S0),
    select(at(Obj,ObjLoc), S0, S1),
    append([at(A,ObjLoc), has(A,Obj)], S1, S2),
    !.

% Recursive story rule that forms the story

% Base case
getStory([],_,_,_).

% Only one goal
getStory([G],S,A,W) :-
    transform(G,S,A),
    do(G,A,W),
    !.

% Loop through many goals
getStory([G|T],S,A,W2) :-
    transform(G,S1,A),
    do(G,A,W1), !,
    append(S1,S2,S),
    getStory(T,S2,W1,W2).

% Text output for each specific action/state

% Create better presentation of locations for the text output
makePretty(S,P) :-
    split_string(S,"_","",L),
    atomic_list_concat(L,' ',A),
    upcase_atom(A,P).

% Base case
textOp([]).

% Recursive call
textOp([H|T]) :- textOp(H), textOp(T).

% Text output for each action/state

textOp(at(A,L)) :-
    write(A),
    write(' is at '),
    makePretty(L,PL),
    write(PL),
    nl.

textOp(has(A,Obj)) :-
    write(A),
    write(' has the '),
    write(Obj),
    nl.

textOp(take(A,Obj)) :-
    write(A),
    write(' takes '),
    write(Obj),
    nl.

textOp(travel(A,D,L)) :-
    write(A),
    write(' moves from '),
    makePretty(D,PD),
    write(PD),
    write(' to '),
    makePretty(L,PL),
    write(PL),
    nl.

% Main story rule, tells the story as text output

% Entry point to generate story with goals, initial state, and final actions
story(Goals, InitialState, FinalActions) :-
    getStory(Goals, InitialState, _, FinalActions).

% Entry point to generate and display the story
story_with_output(Goals, InitialState) :-
    write('Once upon a time, in a faraway kingdom...'), nl,
    textOp(InitialState),
    story(Goals, InitialState, FinalActions),
    textOp(FinalActions).


run_cinderella :-
  story([get(cinderella, slipper), move(cinderella, ballroom)],
    [at(cinderella, kitchen), at(slipper, ballroom)]).

