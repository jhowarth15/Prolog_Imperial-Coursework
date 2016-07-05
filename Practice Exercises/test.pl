:-use_module(library(lists)).


% child_mother(C, M) means C is a child of mother M.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_mother(amy, mary).
child_mother(arthur, mary).
child_mother(angel, jane).
child_mother(anton, rita).
child_mother(alan, rita).
child_mother(axel, susan).
child_mother(ann, tina).    


% age(C, A) means C is of age A.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

age(amy, 6).
age(arthur, 15).
age(angel, 16).
age(anton, 4).
age(alan, 8).
age(axel, 16).
age(ann, 4).

% employed(X) means X is employed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

employed(susan).
employed(rita).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%i.

ecb(M):-
	child_mother(C,M),
	age(C,X),
	X=<14.

ecb(M):-
	child_mother(C,M),
	age(C,X),
	X>14, X=<16,
	\+employed(M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ii.


mother_of_the_youngest(M):-
	setof(X, Y^age(Y,X),[H|_]),
	findall(C, age(C,H), L),
	setof(M1, C1^(member(C1,L),child_mother(C1,M1)), [M|_]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%iii.


mothers_of_young(LM):-
	findall(C, (age(C,X),X=<10), CL),
	setof(M, C1^(member(C1,CL),child_mother(C1,M)),LM).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2.i

%accmerge([],[],L,L).

accmerge([],L2,Acc,L):-
	append(Acc,L2,L).

accmerge(L1,[],Acc,L):-
	append(Acc,L1,L).


accmerge([H1|T1], [H2|T2], Acc, L):-
	H1=<H2, append(Acc,[H1],Acc1),
	accmerge(T1, [H2|T2], Acc1, L).

accmerge([H1|T1], [H2|T2], Acc, L):-
	H2<H1, append(Acc,[H2],Acc1),
	accmerge([H1|T1], T2, Acc1, L).


merge(L1,L2,L):-accmerge(L1,L2,[],L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ii.

accfindElement(N, [H|_], Acc, E):-
	Acc1 is Acc+1,
	Acc1=:=N,
	E = H.
	
accfindElement(N, [_|T], Acc, E):-
	Acc1 is Acc+1,
	Acc1=\=N,
	accfindElement(N,T,Acc1,E).

findElement(N,L,E):-accfindElement(N,L,0,E).














