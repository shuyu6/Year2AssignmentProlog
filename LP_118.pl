% Student ID : 967439 		Student Name : Goh Shu Yu 
% Student ID : 916518 		Student Name : Saskia Davies 


%Question 1 a

%ancestor what is the expected input and output ?

parent(queenmother,elisabeth). 
parent(elisabeth,charles).
parent(elisabeth,andrew). 
parent(elisabeth,anne).
parent(elisabeth,edward). 
parent(diana,william).
parent(diana,harry). 
parent(sarah,beatrice).
parent(anne,peter). 
parent(anne,zara).
parent(george,elisabeth). 
parent(philip,charles).
parent(philip,andrew). 
parent(philip,edward).
parent(charles,william). 
parent(charles,harry).
parent(andrew,beatrice). 
parent(andrew,eugenie).
parent(mark,peter). 
parent(mark,zara).
parent(william,georgejun). 
parent(kate,georgejun).
parent(kate,charlotte).

/*
the_royal_females(queenmother).
the_royal_females(elisabeth).
the_royal_females(anne).
the_royal_females(diana).
the_royal_females(sarah).
the_royal_females(zara).
the_royal_females(kate).
the_royal_females(charlotte).

the_royal_males(charles).
the_royal_males(andrew).
the_royal_males(edward).
the_royal_males(william).
the_royal_males(harry).
the_royal_males(beatrice).
the_royal_males(peter).
the_royal_males(george).
the_royal_males(philip).
the_royal_males(mark).
the_royal_males(eugenie).
the_royal_males(georgejun).*/

the_royal_females([queenmother,elisabeth,anne,diana,sarah,zara,kate,charlotte]).
the_royal_males([charles,andrew,edward,william,harry,beatrice,peter,george,philip,mark,eugenie,georgejun]).
the_royal_family(X):- the_royal_females(Y),the_royal_males(Z),append(Y,Z,X). 
mother(X,Y):- parent(X,Y),the_royal_females(XS),member(X,XS).   %X : mother, Y : child
has_child(X):- parent(X,_).
grandparent(X,Y):-parent(X,Z),parent(Z,Y).

%ancestor(X,Y):- ancestor(X,Y,[],FAMILY),the_royal_family(FAMILY).
%ancestor(_,Y,Y,[]).
%ancestor(X,Y,Z,[FH|FT]):- \+parent(FH,X),ancestor(X,Y,Z,FT).
%ancestor(X,Y,Z,[FH|FT]):- parent(FH,X),member(FH,Z),ancestor(FH,Y,Z,FT).
%ancestor(X,Y,Z,[FH|FT]):- parent(FH,X),\+member(FH,Z),ancestor(FH,Y,[FH,Z],FT).
%ancestor(X,Y,Z,[FH|FT]):- \+parent(FH,X),ancestor(X,Y,Z,FT)

all_ancestor(X,Y):- all_ancestor(X,Y,[]).
all_ancestor(_,Y,Y).
all_ancestor(X,Y,Z):- parent(XP,X),all_ancestor(XP,Y,[XP|Z]).

%mother(X,beatrice).
%X = sarah ;


%the_royal_females(X).
%the_royal_males(X).
%mother(X,beatrice).
%has_child(X).
%grandparent(X,Y).

%Question 1 b
sibling(X,Y):-parent(Z,X),parent(Z,Y),X\=Y,X\=Z,Y\=Z.
aunt(X,Y):-parent(Z,Y),sibling(Z,X),the_royal_females(XS),member(X,XS).


%Question 1 c
starRow(0,0).
starRow(0,N):- N>0 , write(' ') , NewN is N-1 , starRow(0,NewN).
starRow(N,N):- N>0 , write('*') , NewN is N-1 , starRow(NewN,NewN).
starRow(N,M):- N>0 , write('*') , NewN is N-1 , NewM is M-1, starRow(NewN,NewM).

starEndRow(0,0).
starEndRow(N,N):- N>0, write('*') , NewN is N-1 , starEndRow(NewN,NewN).
starEndRow(N,M):- N>0, M>N , write(' '), NewM is M-1, starEndRow(N,NewM).

showPattern(N):-showPattern(N,N).
showPattern(0,_).
showPattern(N,N):- N > 0, starRow(N,N),starEndRow(N,N) ,nl, NewN is N-1 , showPattern(NewN,N),starRow(N,N),starEndRow(N,N),nl.
showPattern(N,M):- N > 0, starRow(N,M),starEndRow(N,M) ,nl, NewN is N-1 , showPattern(NewN,M),starRow(N,M),starEndRow(N,M),nl.

%%%%%%%%%%%%%%%%% Example Output  %%%%%%%%%%%%%%%%%%%%
% ?- showPattern(3).
% ******
% **  **
% *    *
% *    *
% **  **
% ******
% true ;
% false.
%
% ?- showPattern(6).
% ************
% *****  *****
% ****    ****
% ***      ***
% **        **
% *          *
% *          *
% **        **
% ***      ***
% ****    ****
% *****  *****
% ************
% true ;
% false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Question 1 d

factorial(X,Y):- factorial(X,Y,1).
factorial(0,A,A).
factorial(X,Y,A):- X>0, X1 is X-1,NewA is A * X, factorial(X1,Y,NewA).

sumList(XS,O):-sumList(XS,O,0).
sumList([],A,A).
sumList([X|XS],O,A):- NewA is A+X, sumList(XS,O,NewA).

factorialAndProduct(XS,O):-factorialAndProduct(XS,O,1).
factorialAndProduct([],A,A).
factorialAndProduct([X|XS],O,A):- factorial(X,XO),NewA is A * XO, factorialAndProduct(XS,O,NewA).

multinomial(XS,X):- sumList(XS,SUM_N),factorial(SUM_N,UPPER_N),factorialAndProduct(XS,LOWER_N),X is UPPER_N / LOWER_N.

%%%%%%%%%%%% Example Output %%%%%%%%%%%%%%
% ?- multinomial([2,2,2,2],X).
% X = 2520 ;
% false.

% ?- multinomial([3,5,6,7],X).
% X = 19554575040 ;
% false.

% ?- time(multinomial([2500,5200,5020,5020],X)).
% 106,471 inferences, 0.094 CPU in 0.092 seconds (102% CPU, 1135691 Lips)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Question 2 
%K : boat capacity
%AM: missionaries at bank A.
%AC: cannibals at bank A.
%BM: missionaries at bank B.
%BC: cannibals at bank B.
%W : location of weapon. 0 left 1 right 
%B : location of boat.
%L : steps to achieves the goal.

% W = 1 and B = 1 means both of them at bank A
% ?- ferry(3,3,2).
ferry(M,C,K):-state(K,M,C,0,0,1,1,[[K,M,C,0,0,1,1]]).

%stop when bank A is empty, weapon and boat are at bank B
state(_,0,0,_,_,-1,-1,L):- output(L).
state(K,AM,AC,BM,BC,W,B,L):- move([K,AM,AC,BM,BC,W,B],[K,AM1,AC1,BM1,BC1,W1,B1]),
	\+member([K,AM1,AC1,BM1,BC1,W1,B1],L),
	state(K,AM1,AC1,BM1,BC1,W1,B1,[[K,AM1,AC1,BM1,BC1,W1,B1]|L]).
	
%move([2,3,3,0,0,1,1],X).
%no move weapon
%only move massionaries from A to B
/*
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC,BM2,BC,1,-1]):-
	between(1,K,X),
	AM2 is AM - X,
	BM2 is BM + X,
	legalMove(AM2,AC,BM2,BC,1).
	
%only move cabbinal from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM,AC2,BM,BC2,1,-1]):-
	between(1,K,X),
	AC2 is AC - X,
	BC2 is BC + X,
	legalMove(AM,AC2,BM,BC2,1).
	
%move both from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC2,BM2,BC2,1,-1]):-
	between(1,K,X),
	between(1,K,Y),	
	SumXY is X+Y,
	between(1,K,SumXY),
	AC2 is AC - X,
	AM2 is AM - Y,
	BC2 is BC + X,
	BM2 is BM + Y,
	legalMove(AM2,AC2,BM2,BC2,1).

%only move massionaries from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM2,AC,BM2,BC,1,1]):-
	between(1,K,X),
	AM2 is AM + X,
	BM2 is BM - X,
	legalMove(AM2,AC,BM2,BC,1).
	
%only move cabbinal from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM,AC2,BM,BC2,1,1]):-
	between(1,K,X),
	AC2 is AC + X,
	BC2 is BC - X,
	legalMove(AM,AC2,BM,BC2,1).
	
%move both from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM2,AC2,BM2,BC2,1,1]):-
	between(1,K,X),
	between(1,K,Y),	
	SumXY is X+Y,
	between(1,K,SumXY),
	AC2 is AC + X,
	AM2 is AM + Y,
	BC2 is BC - X,
	BM2 is BM - Y,
	legalMove(AM2,AC2,BM2,BC2,1).
*/
%%%%%%%%%%%%%%%%%%%%% moving weapon %%%%%%%%%%%%%%%%%%%%%%%%%%
%only move massionaries from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC,BM2,BC,-1,-1]):-
	between(1,K,X),
	AM2 is AM - X,
	BM2 is BM + X,
	legalMove(AM2,AC,BM2,BC,-1).
		
%move both from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC2,BM2,BC2,-1,-1]):-
	between(1,K,X),
	between(1,K,Y),	
	SumXY is X+Y,
	between(1,K,SumXY),
	AC2 is AC - X,
	AM2 is AM - Y,
	BC2 is BC + X,
	BM2 is BM + Y,
	legalMove(AM2,AC2,BM2,BC2,-1).

%only move massionaries from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM2,AC,BM2,BC,-1,1]):-
	between(1,K,X),
	AM2 is AM + X,
	BM2 is BM - X,
	legalMove(AM2,AC,BM2,BC,-1).
		
%move both from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM2,AC2,BM2,BC2,-1,1]):-
	between(1,K,X),
	between(1,K,Y),	
	SumXY is X+Y,
	between(1,K,SumXY),
	AC2 is AC + X,
	AM2 is AM + Y,
	BC2 is BC - X,
	BM2 is BM - Y,
	legalMove(AM2,AC2,BM2,BC2,-1).

%determine whether that is a legal move 
legalMove(AM,AC,BM,BC,W):- 
	AM>=0,AC>=0,BM>=0,BC>=0,
	(AM>=AC ; W =:= 1 ; AM=:= 0),
	(BM>=BC ; W =:= -1 ; BM=:= 0).

%print out all of the list 	
output([]):- nl.
output([L|LS]):-output(LS), write(L),nl.








