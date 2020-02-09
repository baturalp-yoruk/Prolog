%        KNOWLEDGE BASE 

team(realmadrid, madrid).
team(juventus, torino).
team(galatasaray, istanbul).
team(kobenhavn, copenhagen).
team(manutd, manchester).
team(realsociedad, sansebastian).
team(shaktard, donetsk).
team(bleverkusen, leverkusen).
team(omarseille, marseille).
team(arsenal, london).
team(fcnapoli, napoli).
team(bdortmund, dortmund).

match(1, galatasaray, 1, realmadrid, 6).
match(1, kobenhavn, 1, juventus, 1).
match(1, manutd, 4, bleverkusen, 2).
match(1, realsociedad, 0, shaktard, 2).
match(1, omarseille, 1, arsenal, 2).
match(1, fcnapoli, 2, bdortmund, 1).

match(2, juventus, 2, galatasaray, 2).
match(2, realmadrid, 4, kobenhavn, 0).
match(2, shaktard, 2, manutd, 3).
match(2, bleverkusen, 1, realsociedad, 1).
match(2, bdortmund, 3, omarseille, 0).
match(2, arsenal, 2, fcnapoli, 0).

match(3, galatasaray, 3, kobenhavn, 1).
match(3, realmadrid, 2, juventus, 1).
match(3, manutd, 1, realsociedad, 0).
match(3, bleverkusen, 4, shaktard, 0).
match(3, omarseille, 1, fcnapoli, 2).
match(3, arsenal, 1, bdortmund, 2).

match(4, kobenhavn, 1, galatasaray, 0).
match(4, juventus, 2, realmadrid, 2).
match(4, bleverkusen, 0, manutd, 5).
match(4, shaktard, 4, realsociedad, 0).
match(4, fcnapoli, 4, omarseille, 2).
match(4, bdortmund, 0, arsenal, 1).

match(5, realmadrid, 4, galatasaray, 1).
match(5, juventus, 3, kobenhavn, 1).
match(5, realsociedad, 0, manutd, 0).
match(5, shaktard, 0, bleverkusen, 0).
match(5, bdortmund, 3, fcnapoli, 1).
match(5, arsenal, 2, omarseille, 0).

match(6, galatasaray, 1, juventus, 0).
match(6, kobenhavn, 0, realmadrid, 2).
match(6, manutd, 1, shaktard, 0).
match(6, realsociedad, 2, bleverkusen, 0).
match(6, omarseille, 1, bdortmund, 2).
match(6, fcnapoli, 2, arsenal, 0).

% ALLTEAMS PREDICATE
 
allTeams(L,N):- findall(X,team(X,_),Z), permutation(Z, L), length(L,N).

stableAllTeams(L,N):- findall(X,team(X,_),L), length(L,N).

% WINS PREDICATE AND IT'S HELPER PREDICATES

smaller(A,W):- between(1,W,A).
helper_wins(T,W,L,N):- smaller(A,W), match(A,T,S1,_,S2), S1>S2, findall(X,match(A,T,S1,X,S2),L), length(L,N), W is W.
helper_wins2(T,W,L,N):- smaller(A,W), match(A,_,S1,T,S2), S2>S1, findall(X,match(A,X,S1,T,S2),L), length(L,N), W is W.
helper_wins3(T,W,L,N):- findall(X, helper_wins(T,W,X,N), L).
helper_wins4(T,W,L,N):- findall(X, helper_wins2(T,W,X,N), L).
helper_wins5(T,W,C,N):- helper_wins3(T,W,L1,N), helper_wins4(T,W,L2,N), append(L1,L2,Z), append(Z,C), length(C,N).
wins(T,W,L,N):- helper_wins5(T,W,L,N).

%LOSSES PREDICATE AND IT'S HELPER PREDICATES

helper_losses(T,W,L,N):- smaller(A,W), match(A,T,S1,_,S2), S1<S2, findall(X,match(A,T,S1,X,S2),L), length(L,N), W is W.
helper_losses2(T,W,L,N):- smaller(A,W), match(A,_,S1,T,S2), S2<S1, findall(X,match(A,X,S1,T,S2),L), length(L,N), W is W.
helper_losses3(T,W,L,N):- findall(X, helper_losses(T,W,X,N), L).
helper_losses4(T,W,L,N):- findall(X, helper_losses2(T,W,X,N), L).
helper_losses5(T,W,A,N):- helper_losses3(T,W,L1,N), helper_losses4(T,W,L2,N), append(L1,L2,Z), append(Z,A), length(A,N).
losses(T,W,L,N):- helper_losses5(T,W,L,N).

% DRAWS PREDICATE AND IT'S HELPER PREDICATES

helper_draws(T,W,L,N):- smaller(A,W), match(A,T,S1,_,S2), S1=:=S2, findall(X,match(A,T,S1,X,S2),L), length(L,N), W is W.
helper_draws2(T,W,L,N):- smaller(A,W), match(A,_,S1,T,S2), S2=:=S1, findall(X,match(A,X,S1,T,S2),L), length(L,N), W is W.
helper_draws3(T,W,L,N):- findall(X, helper_draws(T,W,X,N), L).
helper_draws4(T,W,L,N):- findall(X, helper_draws2(T,W,X,N), L).
helper_draws5(T,W,A,N):- helper_draws3(T,W,L1,N), helper_draws4(T,W,L2,N), append(L1,L2,Z), append(Z,A), length(A,N).
draws(T,W,L,N):- helper_draws5(T,W,L,N).

% SCORED PREDICATE AND IT'S HELPER PREDICATES

helper_scored(T,W,L,N,S):- smaller(A,W), match(A,T,S1,_,S2), findall(X,match(A,T,S1,X,S2),L), length(L,N), W is W, S is S1.
helper_scored2(T,W,L,N,S):- smaller(A,W), match(A,_,S1,T,S2), findall(X,match(A,X,S1,T,S2),L), length(L,N), W is W, S is S2.
helper_scored3(T,W,L,N,S):- findall(S, helper_scored(T,W,_,N,S), L).
helper_scored4(T,W,L,N,S):- findall(S, helper_scored2(T,W,_,N,S), L).
scored(T,W,S):- helper_scored3(T,W,L1,N,B), helper_scored4(T,W,L2,N,B), append(L1,L2,Z), sum_list(Z,S).

% CONCEDED PREDICATE AND IT'S HELPER PREDICATES

helper_condeded(T,W,L,N,S):- smaller(A,W), match(A,T,S1,_,S2), findall(X,match(A,T,S1,X,S2),L), length(L,N), W is W, S is S2.
helper_condeded2(T,W,L,N,S):- smaller(A,W), match(A,_,S1,T,S2), findall(X,match(A,X,S1,T,S2),L), length(L,N), W is W, S is S1.
helper_condeded3(T,W,L,N,S):- findall(S, helper_condeded(T,W,_,N,S), L).
helper_condeded4(T,W,L,N,S):- findall(S, helper_condeded2(T,W,_,N,S), L).
conceded(T,W,C):- helper_condeded3(T,W,L1,N,B), helper_condeded4(T,W,L2,N,B), append(L1,L2,Z), sum_list(Z,C).

% AVERAGE PREDICATE

average(T,W,A):- scored(T,W,S), conceded(T,W,C), A is S-C.
 
% ORDER PREDICATE AND IT'S HELPER PREDICATES
 
helper_order(W,A,E) :- stableAllTeams(L,N), Z is N-1, between(0,Z,M), nth0(M,L,X), average(X, W, A), append([A],[X],E).
helper_order2(W,Z) :- findall(X, helper_order(W,_,X),L), sort(L,P), reverse(P,Z). 
helper_order3(W,L) :- helper_order2(W,Z), append(Z,L).
helper_order4(W,Q,X) :- helper_order3(W,L), nth0(X,L,Q).
incr(X,X1) :- X1 is X+2.
helper_order5(W,Q,X) :- helper_order4(W,Z,X), incr(X,X1), stableAllTeams(_,N), X1<(N*2)+2, findall(C, helper_order5(W, C, X1), Z1), append([Z],Z1 , Q).
helper_order6(W,Q,X) :- helper_order5(W,L,X), flatten(L,Q).
order(L,W) :- helper_order6(W,L,1).
 
% TOPTHREE PREDICATE
 
topThree([T1,T2,T3],W) :- order(L1,W), nth0(0,L1,T1), nth0(1,L1,T2), nth0(2,L1,T3).
