/*
Group number: 118 
Saskia Davies 915681
Shu Yu Goh    967439
Ho Nam Michael Lam
*/

%%%%%%%%%%%%%%%%%%%%%%%%% Question 1 a %%%%%%%%%%%%%%%%%%%%%%%%%

% Program: ROYAL
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

the_royal_females([queenmother,elisabeth,anne,diana,sarah,zara,kate,charlotte,beatrice,eugenie]).

the_royal_males([charles,andrew,edward,william,harry,peter,george,philip,mark,georgejun]).

the_royal_family(X):- the_royal_females(Y),the_royal_males(Z),append(Y,Z,X). 

mother(X,Y):- parent(X,Y),the_royal_females(XS),member(X,XS).   %X : mother, Y : child

has_child(X):- parent(X,_).

grandparent(X,Y):-parent(X,Z),parent(Z,Y).

ancestor(X,Y):- parent(X,Y).
ancestor(X,Y):- parent(X,Z), ancestor(Z,Y).

count([],0).
count([_|T],N) :- count(T,N1), N is N1+1.

countDescendants(X,Y):- findall(Z,ancestor(X,Z),L), count(L,Y).

fourOrMoreDecendants(X):- countDescendants(X,Y), Y >= 4.

nameMoreDescendants(X,A):- nameMoreDescendants(X,A,[]).

nameMoreDescendants([],A,A).
nameMoreDescendants([X|XS],A,O) :- \+fourOrMoreDecendants(X),nameMoreDescendants(XS,A,O).
nameMoreDescendants([X|XS],A,O) :- fourOrMoreDecendants(X),nameMoreDescendants(XS,A,[X|O]).

nameMoreDescendants(A):- the_royal_family(X), nameMoreDescendants(X,A).

/*
-------question 8-------
?- mother(X,beatrice).
X = sarah ? ;
no

-------question 9-------
?- nameMoreDescendants(X).
X = [philip, george, elisabeth, queenmother] ;
no

-------question 10-------
ancestor(queenmother,X).
X = elisabeth ? ;
X = charles ? ;
X = andrew ? ;
X = anne ? ;
X = edward ? ;
X = william ? ;
X = harry ? ;
X = georgejun ? ;
X = beatrice ? ;
X = eugenie ? ;
X = peter ? ;
X = zara ? ;
no
*/

%%%%%%%%%%%%%%%%%%%%%%%%% Question 1 b %%%%%%%%%%%%%%%%%%%%%%%%%

sibling(X,Y):- parent(Z,X), parent(Z,Y), X\=Y.

aunt(X,Y):-parent(Z,Y),sibling(Z,X),the_royal_females(XS),member(X,XS).

/*
?- sibling(charles,X).
X = andrew ? ;
X = anne ? ;
X = edward ? ;
X = andrew ? ;
X = edward ? ;
no

?- aunt(X,william).
X = anne ? ;
no
*/

%%%%%%%%%%%%%%%%%%%%%%%%% Question 1 c %%%%%%%%%%%%%%%%%%%%%%%%%
starRow(0).
starRow(N):- N>0 , write('*') , NewN is N-1 , starRow(NewN).

spaceRow(0).
spaceRow(N):- N>0, write('  '), NewN is N-1, spaceRow(NewN).

showPattern(N):- showPattern(N,0).
showPattern(0,_).
showPattern(N,H):- N > 0, starRow(N), spaceRow(H), starRow(N), nl, NewN is N-1, NewH is H+1, showPattern(NewN,NewH),starRow(N), spaceRow(H), starRow(N), nl.
/*
?- showPattern(3).
******
**  **
*    *
*    *
**  **
******
yes.

?- showPattern(6).
************
*****  *****
****    ****
***      ***
**        **
*          *
*          *
**        **
***      ***
****    ****
*****  *****
************
yes.
*/

%%%%%%%%%%%%%%%%%%%%%%%%% Question 1 d %%%%%%%%%%%%%%%%%%%%%%%%%

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

/*
?- multinomial([2,2,2,2],X).
X = 2520.0 ? ;
no
?- multinomial([3,4,5,5],X).
X = 171531360.0 ? ;
no

?- time(multinomial([2500,5200,5020,5020],X)).
% 106,471 inferences, 0.094 CPU in 0.092 seconds (102% CPU, 1135691 Lips)
*/

%%%%%%%%%%%%%%%%%%%%%%%%%% Question 2 %%%%%%%%%%%%%%%%%%%%%%%%%%

%K : boat capacity
%AM: missionaries at bank A.
%AC: cannibals at bank A.
%BM: missionaries at bank B.
%BC: cannibals at bank B.
%W : location of weapon. 1 left -1 right 
%B : location of boat. 1 left -1 right
%L : steps to achieves the goal.

% ?- ferry(3,3,2).
ferry(M,C,K):-state(K,M,C,0,0,1,1,[[K,M,C,0,0,1,1]]).

%stop when bank A is empty weapon and boat are at bank B
state(_,0,0,_,_,-1,-1,L):- output(L).
%W = 1 and B = 1 means both of them at bank A
state(K,AM,AC,BM,BC,W,B,L):- 
	move([K,AM,AC,BM,BC,W,B],[K,AM1,AC1,BM1,BC1,W1,B1]),
	\+member([K,AM1,AC1,BM1,BC1,W1,B1],L),
	state(K,AM1,AC1,BM1,BC1,W1,B1,[[K,AM1,AC1,BM1,BC1,W1,B1]|L]).
	
%no moving weapon
%only move massionaries from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC,BM2,BC,1,-1]):-
	myBetween(1,K,X),
	AM2 is AM - X,
	BM2 is BM + X,
	legalMove(AM2,AC,BM2,BC,1).
	
%only move cabbinal from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM,AC2,BM,BC2,1,-1]):-
	myBetween(1,K,X),
	AC2 is AC - X,
	BC2 is BC + X,
	legalMove(AM,AC2,BM,BC2,1).
	
%move both from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC2,BM2,BC2,1,-1]):-
	myBetween(1,K,X),
	myBetween(1,K,Y),	
	SumXY is X+Y,
	myBetween(1,K,SumXY),
	AC2 is AC - X,
	AM2 is AM - Y,
	BC2 is BC + X,
	BM2 is BM + Y,
	legalMove(AM2,AC2,BM2,BC2,1).

%only move massionaries from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM2,AC,BM2,BC,1,1]):-
	myBetween(1,K,X),
	AM2 is AM + X,
	BM2 is BM - X,
	legalMove(AM2,AC,BM2,BC,1).
	
%only move cabbinal from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM,AC2,BM,BC2,1,1]):-
	myBetween(1,K,X),
	AC2 is AC + X,
	BC2 is BC - X,
	legalMove(AM,AC2,BM,BC2,1).
	
%move both from B to A
move([K,AM,AC,BM,BC,1,-1],[K,AM2,AC2,BM2,BC2,1,1]):-
	myBetween(1,K,X),
	myBetween(1,K,Y),	
	SumXY is X+Y,
	myBetween(1,K,SumXY),
	AC2 is AC + X,
	AM2 is AM + Y,
	BC2 is BC - X,
	BM2 is BM - Y,
	legalMove(AM2,AC2,BM2,BC2,1).

%%%%%%%%%%%%%%%%%%%%% moving weapon %%%%%%%%%%%%%%%%%%%%%%%%%%
%only move massionaries from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC,BM2,BC,-1,-1]):-
	myBetween(1,K,X),
	AM2 is AM - X,
	BM2 is BM + X,
	legalMove(AM2,AC,BM2,BC,-1).
		
%move both from A to B
move([K,AM,AC,BM,BC,1,1],[K,AM2,AC2,BM2,BC2,-1,-1]):-
	myBetween(1,K,X),
	myBetween(1,K,Y),	
	SumXY is X+Y,
	myBetween(1,K,SumXY),
	AC2 is AC - X,
	AM2 is AM - Y,
	BC2 is BC + X,
	BM2 is BM + Y,
	legalMove(AM2,AC2,BM2,BC2,-1).

%only move massionaries from B to A
move([K,AM,AC,BM,BC,-1,-1],[K,AM2,AC,BM2,BC,1,1]):-
	myBetween(1,K,X),
	AM2 is AM + X,
	BM2 is BM - X,
	legalMove(AM2,AC,BM2,BC,1).
		
%move both from B to A
move([K,AM,AC,BM,BC,-1,-1],[K,AM2,AC2,BM2,BC2,1,1]):-
	myBetween(1,K,X),
	myBetween(1,K,Y),	
	SumXY is X+Y,
	myBetween(1,K,SumXY),
	AC2 is AC + X,
	AM2 is AM + Y,
	BC2 is BC - X,
	BM2 is BM - Y,
	legalMove(AM2,AC2,BM2,BC2,1).

%determine whether that is a legal move 
%legalMove(3,1,0,2,-1).
legalMove(AM,AC,BM,BC,W):- 
	%all should be positive number 
	AM>=0,AC>=0,BM>=0,BC>=0,
	(AM>=AC ; (AM =\= 0, AC >= AM, W =:= 1) ; (AM =:= 0, W =:= -1)),
	(BM>=BC ; (BM =\= 0, BC >= BM, W =:= -1) ; (BM =:= 0, W =:= 1)).

%print out all of the output list 	
output([]):- nl.
output([L|LS]):-output(LS), write(L),nl.

%check Z whether is between X and Y
myBetween(X,Y,X):- X=<Y.
myBetween(X,Y,Z):- X<Y, NewX is X+1, myBetween(NewX, Y, Z).


/*
?- ferry(3,3,2).

[2,3,3,0,0,1,1]
[2,1,3,2,0,1,-1]
[2,2,3,1,0,1,1]
[2,2,2,1,1,1,-1]
[2,3,2,0,1,1,1]
[2,1,2,2,1,1,-1]
[2,2,2,1,1,1,1]
[2,1,1,2,2,1,-1]
[2,1,2,2,1,1,1]
[2,0,1,3,2,-1,-1]
[2,1,1,2,2,1,1]
[2,0,0,3,3,-1,-1]
yes
*/