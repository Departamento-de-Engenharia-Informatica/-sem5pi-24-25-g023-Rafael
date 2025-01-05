:- dynamic surgery_id/2.
:- dynamic surgery_penalty/3.
:- dynamic max_time/1.
:-dynamic generations/1.
:-dynamic value_limit/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:-dynamic selection_method/1.
:- dynamic stability_generations/1.
 
agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(850,900,m02),(901,960,m02),(1380,1440,c02)]).
agenda_staff(d003,20241028,[(720,790,m01),(910,980,m02)]).
agenda_staff(d004,20241028,[(850,900,m02),(940,980,c04)]).

timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).
timetable(d004,20241028,(620,1020)).

staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003,doctor,orthopaedist,[so2,so3,so4]).
staff(d004,doctor,orthopaedist,[so2,so3,so4]).

%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning).
surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so4).

assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).
assignment_surgery(so100004,d001).
assignment_surgery(so100004,d002).
assignment_surgery(so100005,d002).

agenda_operation_room(or1,20241028,[(520,579,so100000),(1000,1059,so099999)]).
agenda_operation_room(or2,20241028,[]).
agenda_operation_room(or3,20241028,[]).
 
% parameters initialization
initialize:-
write('Number of generations: '),read(NG),
    (retract(generations(_));true), asserta(generations(NG)),

    write('Population size: '),read(PS),
    (retract(population(_));true), asserta(population(PS)),
  
    write('Probability of crossover (%):'), read(P1), PC is P1/100,
    (retract(prob_crossover(_));true),    asserta(prob_crossover(PC)),
  
    write('Probability of mutation (%):'), read(P2),PM is P2/100,
    (retract(prob_mutation(_));true), asserta(prob_mutation(PM)),

    write('Selection method (elitist/non-elitist):'), read(SM), (SM = elitist ; SM = non-elitist),
    (retract(selection_method(_));true), asserta(selection_method(SM)),

    % Stop conditions
    write('Maximum runtime (seconds): '),read(Time),
    (retract(max_time(_));true), asserta(max_time(Time)),
  
    write('Best value to stop:'), read(ValidValue),
    (retract(value_limit(_));true), asserta(value_limit(ValidValue)),
  
    write('Number of generations for pop stability:'), read(SG),
    (retract(stability_generations(_));true), asserta(stability_generations(SG)).
 
 
generate:-
  initialize,
  get_time(StartTime),
  generate_population(Pop),
  write('Pop='),write(Pop),nl,
  evaluate_population(Pop,PopValue),
  write('PopValue='),write(PopValue),nl,
  order_population(PopValue,PopOrd),
  generations(NG),
  value_limit(ValidValue),
  max_time(MaxTime),
  stability_generations(StabilityGenerations),
  generate_generation(0,NG,ValidValue,PopOrd,0,StartTime,MaxTime,StabilityGenerations).
 
 
generate_population(Pop):-
  population(PopSize),
  count_surgeries(NumT),
  create_surgery_penalties,
  findall(Surgery,surgery_penalty(Surgery,_,_),SurgeriesList),
  generate_population(PopSize,SurgeriesList,NumT,Pop).
 
 
% Count the number of surgeries
count_surgeries(Count) :-
  findall((A, B), surgery_id(A, B), List),
  length(List, Count).
 
 
% Add the surgery penalty fact
create_surgery_penalties :-
  forall(surgery_id(Surgery, Type),
  assertz(surgery_penalty(Surgery, Type, 0))).
 
 
generate_population(0,_,_,[]):-!.
generate_population(PopSize,SurgeriesList,NumT,[Ind|Rest]):-
  PopSize1 is PopSize-1,
  generate_population(PopSize1,SurgeriesList,NumT,Rest),
  generate_individual(SurgeriesList,NumT,Ind),
  not(member(Ind,Rest)).
generate_population(PopSize,SurgeriesList,NumT,L):-
  generate_population(PopSize,SurgeriesList,NumT,L).
 
 
generate_individual([G],1,[G]):-!.
 
generate_individual(SurgeriesList,NumT,[G|Rest]):-
  NumTemp is NumT + 1, % to use with random
  random(1,NumTemp,N),
  remove(N,SurgeriesList,G,NewList),
  NumT1 is NumT-1,
  generate_individual(NewList,NumT1,Rest).
 
 
remove(1,[G|Rest],G,Rest).
remove(N,[G1|Rest],G,[G1|Rest1]):- N1 is N-1,
  remove(N1,Rest,G,Rest1).
 
 
evaluate_population([],[]).
evaluate_population([Ind|Rest],[Ind*V|Rest1]):-
  evaluate(Ind,V),
  evaluate_population(Rest,Rest1).
 
 
evaluate(Seq,V):- evaluate(Seq,0,V).
 
evaluate([ ],_,0).

evaluate([Surgery|Rest], TotalTime, V) :-
  value_limit(ValidValue),
  surgery_penalty(Surgery, SurgeryName, Penalty),
  surgery_time(SurgeryName, SurgeryTime),
 
  NewTotalTime is TotalTime + SurgeryTime,
 
  ( NewTotalTime =< ValidValue ->
    evaluate(Rest, NewTotalTime, VRest),
    V is NewTotalTime + VRest + Penalty
  ; 
    ExceedPenalty is Penalty + (NewTotalTime - ValidValue),
    evaluate(Rest, NewTotalTime, VRest),
    V is NewTotalTime + VRest + ExceedPenalty
  ).
 
 
% Calculate the total time of a surgery
surgery_time(SurgeryName, TotalTime) :-
  surgery(SurgeryName, Time1, Time2, Time3),
  TotalTime is Time1 + Time2 + Time3.
 
 
order_population(PopValue, PopValueOrd) :-
  sort(2, @>=, PopValue, PopValueOrd).
 
 
% Generate the next generation
generate_generation(_, _, _, Pop, Counter, StartTime, MaxTime,_) :-
  get_time(CurrentTime),
  ElapsedTime is CurrentTime - StartTime,
  ElapsedTime > MaxTime, !,  % Stop condition if elapsed time exceeds MaxTime
  write('Runtime limit met.'), nl,
  write('Final Generation '), write(Counter), write(':'), nl, write(Pop), nl.

generate_generation(G, G, _, Pop, Counter, _, _,_) :- !,
  nl, write('Final Generation '), write(Counter), write(':'), nl, write(Pop), nl.

generate_generation(N, G, ValidValue, Pop, Counter, StartTime, MaxTime, MaxStabilizationGenerations) :-
  nl, write('Generation '), write(Counter), write(':'), nl, write(Pop), nl,

  Pop = [Best*BestValue|_],
  write('Best individual: '), write(Best), write(' with Value: '), write(BestValue), nl,

  % Perform genetic operations with random permutation
  random_permutation(Pop, ShuffledPop),
  crossover(ShuffledPop, NPop1),
  mutation(NPop1, NPop),

  % Merge current population with descendants
  append(Pop, NPop, MergedPopulation),
  % Remove duplicates
  sort(MergedPopulation, UniquePopulation),

  evaluate_population(UniquePopulation, NPopValue),
  order_population(NPopValue, NPopOrd),

  % Current and New population
  NPopOrd = [_NewBest*NewBestValue|_],
  (NewBestValue @=< BestValue
    -> FinalPop = NPopOrd,
      (BestValue @=< ValidValue -> N1 is N + 1 ; N1 is 0),
      nl, write('NewBest value:'), write(NewBestValue), nl
    ;  
      selection_method(Method),
      (Method = elitist -> 
        elitist_method(Best*BestValue, NPopOrd, FinalPop)
      ; 
        non_elitist_method(Best*BestValue, NPopOrd, FinalPop)
      )
  ),
  NewCounter is Counter + 1,

  (BestValue =< ValidValue -> 
        write('Value limit met at value'), write(BestValue), nl

      ; (N1 >= MaxStabilizationGenerations -> 
          write('Population stabilized at '), write(MaxStabilizationGenerations), write(' generations'), 
          nl, write('Final Population: '), write(FinalPop), nl
        ;
        generate_generation(N1, G, ValidValue, FinalPop, NewCounter, StartTime, MaxTime, MaxStabilizationGenerations)
      )
  ).

% Elitist selection method
elitist_method(Best*BestValue, Population, FinalPopulation) :-
  add_best(Best*BestValue, Population, FinalPopulation).

% Non-elitist selection method
non_elitist_method(Best*BestValue, Population, FinalPopulation) :-
  random(0.0, 1.0, R),  % Generate random number between 0 and 1
  (R < 0.5 ->
    % Keep the best individual
    add_best(Best*BestValue, Population, FinalPopulation)
  ;
    % Tournament selection
    tournament_selection(Population, BestSelected, WorstSelected),
    random(0.0, 1.0, R2),
    % 20% chance to keep the worst individual
    (R2 < 0.2 ->
      Selected = WorstSelected  % Select worst from tournament
    ;
      Selected = BestSelected  % Select best from tournament
    ),
    add_best(Selected, Population, FinalPopulation)
  ).

% Tournament selection for non-elitist approach
tournament_selection(Population, BestSelected, WorstSelected) :-
  % Get random individuals for comparison
  length(Population, PopLen),
  random(0, PopLen, Indx1),
  random(0, PopLen, Indx2),
  nth0(Indx1, Population, Ind1*Val1),
  nth0(Indx2, Population, Ind2*Val2),
  % Select the better and worse individual
  (Val1 =< Val2 -> 
    (BestSelected = Ind1*Val1, WorstSelected = Ind2*Val2)
  ; 
    (BestSelected = Ind2*Val2, WorstSelected = Ind1*Val1)
  ).

add_best(Best*BestValue, Population, FinalPopulation) :-
  append(Front, [_|Rest], Population),
  append(Front, Rest, TempPopulation),
  append([Best*BestValue], TempPopulation, FinalPopulation).

% Non-elitist selection using tournament
% include_best_non_elitist(Best*BestValue, Population, FinalPopulation) :-
%   % Merge current population with descendants
%   append(Population, [Best*BestValue], MergedPopulation),
%   % Remove duplicates
%   sort(MergedPopulation, UniquePopulation),
%   % Sort by evaluation
%   sort(2, @=<, UniquePopulation, SortedPopulation),
%   % Select top P individuals
%   P is round(0.2 * length(SortedPopulation)),
%   length(TopP, P),
%   append(TopP, Rest, SortedPopulation),
%   % Create new list with remaining individuals
%   findall(Ind*Eval*Rand, (member(Ind*Eval, Rest), random(0.0, 1.0, Rand)), RandList),
%   % Sort by product of evaluation and random number
%   sort(3, @=<, RandList, SortedRandList),
%   % Select first N-P individuals
%   length(NextGen, N-P),
%   append(NextGen, _, SortedRandList),
%   % Combine top P and next N-P individuals
%   append(TopP, NextGen, FinalPopulation).

generate_crossover_points(P1,P2):-
  count_surgeries(N),
  NTemp is N+1,
  random(1,NTemp,P11),
  random(1,NTemp,P21),
  P11\==P21,!,
  ((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).

crossover([], [], _).
crossover([Ind*_], [Ind], _).
crossover([Ind1*_, Ind2*_|Rest], [NInd1, NInd2|Rest1]) :-
  random(0, 2, Rand),

  length(Rest, Len),
  (Len > 0 ->
    random(0, Len, RandomIndex),
    nth0(RandomIndex, Rest, NextInd*_)
    ; NextInd = Ind2
  ),

  (Rand =:= 0 ->
    generate_crossover_points(P1, P2),
    prob_crossover(Pcruz),
    random(0.0, 1.0, Pc),
    (Pc =< Pcruz ->
      cross(Ind1, NextInd, P1, P2, NInd1),
      cross(NextInd, Ind1, P1, P2, NInd2)
    ;
      NInd1 = Ind1, NInd2 = NextInd
    )
  ;
    NInd1 = Ind1, NInd2 = Ind2
  ),
  crossover(Rest, Rest1).

% Helper predicates
last([X], X) :- !.
last([_|T], X) :- last(T, X).
 
nth0(0, [H|_], H) :- !.
nth0(N, [_|T], X) :- N1 is N-1, nth0(N1, T, X).
 
fillh([ ],[ ]).
 
fillh([_|R1],[h|R2]):-
  fillh(R1,R2).
 
sublist(L1,I1,I2,L):-I1 < I2,!,
  sublist1(L1,I1,I2,L).
 
sublist(L1,I1,I2,L):-sublist1(L1,I2,I1,L).
 
sublist1([X|R1],1,1,[X|H]):-!, fillh(R1,H).
 
sublist1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
  sublist1(R1,1,N3,R2).
 
sublist1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
  N4 is N2 - 1,
  sublist1(R1,N3,N4,R2).
 
rotate_right(L,K,L1):- count_surgeries(N),
  T is N - K,
  rr(T,L,L1).
 
rr(0,L,L):-!.
 
rr(N,[X|R],R2):- N1 is N - 1,
  append(R,[X],R1),
  rr(N1,R1,R2).
 
remove([],_,[]):-!.
 
remove([X|R1],L,[X|R2]):- not(member(X,L)),!,
  remove(R1,L,R2).
 
remove([_|R1],L,R2):-
  remove(R1,L,R2).
 
insert([],L,_,L):-!.
insert([X|R],L,N,L2):-
  count_surgeries(T),
  ((N>T,!,N1 is N mod T);N1 = N),
  insert1(X,N1,L,L1),
  N2 is N + 1,
  insert(R,L1,N2,L2).
 
 
insert1(X,1,L,[X|L]):-!.
insert1(X,N,[Y|L],[Y|L1]):-
  N1 is N-1,
  insert1(X,N1,L,L1).
 
cross(Ind1,Ind2,P1,P2,NInd11):-
  sublist(Ind1,P1,P2,Sub1),
  count_surgeries(NumT),
  R is NumT-P2,
  rotate_right(Ind2,R,Ind21),
  remove(Ind21,Sub1,Sub2),
  P3 is P2 + 1,
  insert(Sub2,Sub1,P3,NInd1),
  removeh(NInd1,NInd11).
 
 
removeh([],[]).
 
removeh([h|R1],R2):-!,
  removeh(R1,R2).
 
removeh([X|R1],[X|R2]):-
  removeh(R1,R2).
 
mutation([],[]).
mutation([Ind|Rest],[NInd|Rest1]):-
  prob_mutation(Pmut),
  random(0.0,1.0,Pm),
  ((Pm < Pmut,!,mutation1(Ind,NInd));NInd = Ind),
  mutation(Rest,Rest1).
 
mutation1(Ind,NInd):-
  generate_crossover_points(P1,P2),
  mutation22(Ind,P1,P2,NInd).
 
mutation22([G1|Ind],1,P2,[G2|NInd]):-
  !, P21 is P2-1,
  mutation23(G1,P21,Ind,G2,NInd).
mutation22([G|Ind],P1,P2,[G|NInd]):-
  P11 is P1-1, P21 is P2-1,
  mutation22(Ind,P11,P21,NInd).
 
mutation23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutation23(G1,P,[G|Ind],G2,[G|NInd]):-
  P1 is P-1,
  mutation23(G1,P1,Ind,G2,NInd).