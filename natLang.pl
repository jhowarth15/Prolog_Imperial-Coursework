sentence(S):- noun_phrase(NP), verb_phrase(VP), append(NP, VP, S).
 

noun_phrase(NP):- article(A), noun(N), append(A, N, NP). 

verb_phrase(V):- verb(V).
verb_phrase(VP):- verb(V), noun_phrase(NP), append(V, NP, VP).

% The Lexicon: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

article([the]).
article([a]).
article([an]).

noun([grass]).
noun([cow]).
noun([girl]).
noun([boy]).
noun([apple]).
noun([song]).

verb([eats]).
verb([sings]).
verb([chews]).
verb([kicks]).

adverb([slowly]).
adverb([deliberately]).
adverb([merrily]).
adverb([sweetly]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1.a

% The number of sentences in text = 1 + number of ands.
% Count number of instances in list count(List, Instance, Count).
count([],H,1).
count([H|T],H,Y):- count(T,H,Z), Y is 1+Z.
count([X|T],H,Z):- X\=H,count(T,H,Z).

count_sentences(Text, Count):-
	count(Text, 'and', Count).

% Test.
% count_sentences([the, boy, chews, an, apple], X).
% count_sentences([the, boy, chews, an, apple, and, the, girl, kicks, the, boy], X).
% count_sentences([the, girl, chews, an, apple, and, the, girl, sings, a, song, and, the, girl, kicks, the, cow ], X).

% 1.b

% find the word Y that follows X in the list.
followedBy(X,Y,L) :- append(_,[X,Y|_],L).	

% Get a list of the word after each occurence of actor.
actions(Actor, Text, As):-
	findall(A,(followedBy(Actor,A,Text),verb([A])),As).

% Test.
% actions(boy, [the, boy, chews, an, apple, and, the, girl, kicks, the, boy], X).
% actions(apple, [the, boy, chews, an, apple, and, the, girl, kicks, the, boy], X).
% actions(girl, [the, girl, chews, an, apple, and, the, girl, sings, a, song, and, the, girl, kicks, the, cow ], X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2.

vowel(X):-
	member(X,[a,e,i,o,u]).

% Special case is where a word begins with a vowel.
special_case(X):-
	member(X,[[an],[the]]).

normal_case(X):-
	member(X,[[a],[the]]).

% See if the first letter is a vowel.
first_char_vowel(W):-
	atom_chars(W,[H|_]),
	vowel(H).

% Create noun phrase using an or the if it is the special case.
noun_phrase_better(NP):-
	article(A), noun(N), N=[N1],
	first_char_vowel(N1), special_case(A),
	append(A, N, NP);
	article(A), noun(N), N=[N1],
	\+(first_char_vowel(N1)), normal_case(A),
	append(A, N, NP).

% test
% noun_phrase_better(X).
% noun_phrase_better([a, apple]).
% noun_phrase_better([an, apple]).
% noun_phrase_better([an, cow]).
% noun_phrase_better([a, X]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3.a,b

non_member(X,L):-
	\+member(X,L).

% Conjunction of adverbs
% can be a single adverb.

% Enable cadvs() to take 2 parameters.
cadvs(L):- cadvs(L,[]).

% Set the base case of one adverb.
% Use accumulator to check adverb not already used.
cadvs(L, Acc):- 
	adverb(L), [M]=L,
	non_member(M,Acc).

% test
% cadvs([slowly]). yes
% cadvs([no]). no
% cadvs([slowly],[slowly]). no
% cadvs([slowly],[sweetly]). yes
% cadvs([slowly],[sweetly,slowly]). no

% Case where there is a comma after adverb. Tail must not be last adverb in the list ie. not an adverb.
cadvs([H,','|T], Acc):- 
	adverb([H]), \+adverb(T),
	non_member(H,Acc),
	append(Acc,[H],Acc1), 
	cadvs(T,Acc1).

% Case where there is an and after adverb. Tail must be last adverb ie. an adverb.
cadvs([H,and|[T]],Acc):- 
	adverb([H]), adverb([T]),
	non_member(H,Acc),
	append(Acc,[H],Acc1), 
	cadvs([T],Acc1).

% test
% cadvs([slowly, and, merrily]). yes
% cadvs([slowly, and, slowly]). no
% cadvs([slowly,',',merrily, ',',sweetly]). no
% cadvs([slowly,',',merrily, ',',sweetly,',',slowly]). no
% cadvs([slowly,',', merrily, and, sweetly]). yes
% cadvs([slowly, and, merrily, and, sweetly]). no
% cadvs([slowly,',', merrily, and, slowly]). no
% cadvs([slowly,',', merrily, ',', sweetly]). no
% cadvs([slowly,',', deliberately, ',', merrily, and, sweetly]). yes


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.c

% Just a verb.
verb_phrase_better(V):- verb(V).

% Verb and better noun phrase.
verb_phrase_better(VP):- verb(V), noun_phrase_better(NP), append(V, NP, VP).

% Conjunction of adverbs and verb.
verb_phrase_better(VP):-  append(AV, V, VP), cadvs(AV), verb(V).

% Conjunction of adverbs, verb and better noun phrase.
verb_and_nounphrase(VPN):- append(V, NP, VPN), verb(V), noun_phrase_better(NP).
verb_phrase_better(VP):- append(AV, VPN, VP), cadvs(AV), verb_and_nounphrase(VPN).


% test
% verb_phrase_better([sings]). yes
% verb_phrase_better([sweetly, sings]). yes
% verb_phrase_better([slowly,',', merrily, and, sweetly, sings]). yes
% verb_phrase_better([slowly,',', merrily, and, sweetly, sings, a, song]). yes
% verb_phrase_better([sings, a, song]). yes
% verb_phrase_better([sings, an, song]). no
% verb_phrase_better([slowly,',', merrily, and, slowly, sings, a, song]). no

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.d

sentence_better(S):- append(NP, VP, S), noun_phrase_better(NP), verb_phrase_better(VP).

% test
% sentence_better( [a, boy, slowly,',', merrily, and, sweetly, eats, a, apple]). no
% sentence_better( [a, boy, slowly,',', merrily, and, sweetly, eats, an, apple]). yes
% sentence_better( [a, apple, slowly,',', merrily, and, sweetly, eats, an, apple]). no










