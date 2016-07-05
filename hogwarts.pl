%1.

% the students in Hogwarts defined as student(SID, SN, House)
student(hp, 'Harry James Potter', gryffindor).
student(hg, 'Hermione Jean Granger', gryffindor).
student(rw, 'Ronald Weasley', gryffindor).
student(ll, 'Luna Lovegood', ravenclaw).
student(cc, 'Cho Chang', ravenclaw).
student(tb, 'Terry Boot', ravenclaw).
student(ha, 'Hannah Abbott', hufflepuff).
student(cd, 'Cedric Diggory', hufflepuff).
student(nt, 'Nymphadora Tonks',hufflepuff).
student(dm, 'Draco Malfoy', slytherin).
student(gg, 'Gregory Goyle', slytherin).
student(vc, 'Vincent Crabbe', slytherin).
student(jh, 'Joshua Howarth', hufflepuff).

% the teachers in Hogwarts defined as teacher(TID, TN)
teacher(ad, 'Albus Percival Wulfric Brian Dumbledore').
teacher(ff, 'Filius Flitwick').
teacher(rh, 'Rubeus Hagrid').
teacher(gl, 'Gilderoy Lockhart').
teacher(rl, 'Remus John Lupin').
teacher(mm, 'Minerva McGonagall').
teacher(qq, 'Quirinus Quirrell').
teacher(ss, 'Severus Snape').
teacher(ps, 'Pomona Sprout').
teacher(st, 'Sibyll Patricia Trelawney').
teacher(mh, 'Madam Hooch').
teacher(as, 'Aurora Sinistra').
teacher(cub, 'Cuthbert Binns').
teacher(bb, 'Bathsheba Babbling').
teacher(sv, 'Septima Vector').
teacher(chb, 'Charity Burbage').
teacher(wt, 'Wilkie Twycross').

% compulsory courses for the MSc in Magic defined as compCourse(SCN, CN, TID)
compCourse(astro, 'Astronomy', as).
compCourse(charms, 'Charms', ff).
compCourse(defence, 'Defence against the Dark Arts', qq).
compCourse(fly, 'Flying', mh).
compCourse(herb, 'Herbology', ps).
compCourse(history, 'History of Magic', cub).
compCourse(potions, 'Potions', ss).
compCourse(trans, 'Transfiguration', mm).

% optional courses for the MSc in Magic defined as optCourse(SCN, CN, TID)
optCourse(runes, 'Study of Ancient Runes', bb).
optCourse(arith, 'Arithmancy', sv).
optCourse(muggle, 'Muggle Studies', chb).
optCourse(creatures, 'Care of Magical Creatures', rh).
optCourse(div, 'Divination', st).
optCourse(app, 'Apparition', wt).
optCourse(choir, 'Frog Choir', ff).
optCourse(quid, 'Quidditch', mh).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2.

% optional courses students take defined as enrolled_opt(SID, SCN)
enrolled_opt(hp, runes).
enrolled_opt(hp, muggle).
enrolled_opt(hp, quid).
enrolled_opt(hg, arith).
enrolled_opt(hg, muggle).
enrolled_opt(hg, div).
enrolled_opt(hg, app).
enrolled_opt(hg, choir).
enrolled_opt(hg, runes).
enrolled_opt(rw, quid).
enrolled_opt(rw, app).
enrolled_opt(rw, choir).
enrolled_opt(ll, choir).
enrolled_opt(ll, creatures).
enrolled_opt(ll, div).
enrolled_opt(cc, quid).
enrolled_opt(cc, arith).
enrolled_opt(cc, choir).
enrolled_opt(tb, runes).
enrolled_opt(tb, quid).
enrolled_opt(tb, creatures).
enrolled_opt(ha, app).
enrolled_opt(ha, muggle).
enrolled_opt(ha, quid).
enrolled_opt(cd, arith).
enrolled_opt(cd, app).
enrolled_opt(cd, quid).
enrolled_opt(nt, muggle).
enrolled_opt(nt, app).
enrolled_opt(nt, choir).
enrolled_opt(dm, app).
enrolled_opt(dm, arith).
enrolled_opt(dm, creatures).
enrolled_opt(gg, creatures).
enrolled_opt(gg, arith).
enrolled_opt(gg, app).
enrolled_opt(vc, creatures).
enrolled_opt(vc, arith).
enrolled_opt(vc, app).
enrolled_opt(jh, app).
enrolled_opt(jh, quid).
enrolled_opt(jh, creatures).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%3.

% student SID does course SCN if enrolled_opt or course is compulsory
enrolled(SID, SCN):-
	enrolled_opt(SID, SCN);
	compCourse(SCN, _, _).

% test
% enrolled(jh, X).
% enrolled(X, quid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%4.

% teacher name TN teaches course short name SCN
teaches(TN, SCN):-
	teacher(TID, TN),
	(compCourse(SCN, _, TID);
	optCourse(SCN, _, TID)).

% test
% teaches('Severus Snape', X).
% teaches('Filius Flitwick', X).
% teaches(X, creatures).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%5.

% student name SN taught by teacher name TN
taughtBy(SN, TN):-
	student(SID, SN, _),
	enrolled(SID, SCN),
	teaches(TN, SCN).

% test
% taughtBy('Joshua Howarth', X).
% taughtBy(X, 'Severus Snape').
% taughtBy('Harry James Potter', 'Albus Percival Wulfric Brian Dumbledore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%6.

% student SN takes optional course called CN
takesOption(SN, CN):-
	enrolled_opt(SID, SCN),
	student(SID, SN, _),
	optCourse(SCN, CN, _).

% test
% takesOption(X, 'Quidditch').
% takesOption('Joshua Howarth', X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%7.

% student SN takes alphabetical list of OptCourses
takesAllOptions(SN, OptCourses):-
	setof(CN, takesOption(SN, CN), OptCourses).

% test
% takesAllOptions('Hermione Jean Granger', X).
% takesAllOptions(X, ['Apparition','Frog Choir','Quidditch']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%8.
% convert SID to SN
convert(SID, SN):-
	student(SID, SN, _).

% replace members of SID List with list of respective SN
replace([],[]).
replace([SID_Head|SID_Tail], [SN_Head|SN_Tail]):-
	convert(SID_Head, SN_Head),
	replace(SID_Tail, SN_Tail).

% establish valid houses
house(gryffindor).
house(hufflepuff).
house(ravenclaw).
house(slytherin).

% list of Students names (alphabetically ordered by ID) in House
studentsInHouse(House, Students):-	
	setof(SID, SN^student(SID, SN, House), List),
	replace(List, Students),
	house(House);
	% or if the list is empty but the house is still valid:
	\+(setof(SID, SN^student(SID, SN, House), List)),
	house(House),
	Students = [].

% test 
% studentsInHouse(gryffindor, X).
% studentsInHouse(minions, X). [To test if invalid house throws 'no' answer.]
% studentsInHouse(ravenclaw, X). [With ravenclaw members moved to another house to get empty list result.]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%9.

% helper functions of just students on course in particular house
takesCourse(SN, SCN, CN, House):-
	student(SID, SN, House),
	enrolled(SID, SCN),
	(compCourse(SCN, CN, _);
	optCourse(SCN, CN, _)).

studentsOnCourseHelper(SCN, CN, House, Students):-
	findall(SN, takesCourse(SN, SCN, CN, House), Students).

studentsOnCourse(SCN, CN, StudentsByHouse):-
	StudentsByHouse = [gryffindor-G, hufflepuff-H, ravenclaw-R, slytherin-S],
	studentsOnCourseHelper(SCN, CN, gryffindor, G),
	studentsOnCourseHelper(SCN, CN, hufflepuff, H),
	studentsOnCourseHelper(SCN, CN, ravenclaw, R),
	studentsOnCourseHelper(SCN, CN, slytherin, S),
	takesCourse(_, SCN, CN, _).

% test
% takesCourse('Joshua Howarth', quid, 'Quidditch', hufflepuff).
% studentsOnCourseHelper(quid, CN, hufflepuff, Students).
% studentsOnCourse(quid, 'Quidditch', StudentsByHouse).
% studentsOnCourse(app, X, StudentsByHouse).
% studentsOnCourse(X, 'Flying', StudentsByHouse).
% studentsOnCourse(pl, 'Prolog', X). [To check that it returns false for made up courses]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%10.

% optional course CN is taken by student SN1 and SN2
sharedCourse(SN1, SN2, CN):-
	takesOption(SN1, CN),
	takesOption(SN2, CN),
	SN1\=SN2.

% test 
% sharedCourse(SN1, SN2, 'Quidditch').
% sharedCourse('Joshua Howarth', 'Harry James Potter', CN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%11.

% students SN1 and SN2 are enrolled on the same 3 optional Courses
sameOptions(SN1, SN2, Courses):-
	sharedCourse(SN1, SN2, CN1),
	sharedCourse(SN1, SN2, CN2),
	sharedCourse(SN1, SN2, CN3),
	CN1\=CN2,
	CN2\=CN3,
	CN1\=CN3,
	Courses = [CN1| [CN2, CN3]].

% test
% sameOptions('Draco Malfoy', 'Vincent Crabbe', Courses).
% sameOptions('Hermione Jean Granger', X, Courses).
% sameOptions(Y, X, ['Apparition'| ['Care of Magical Creatures', 'Arithmancy']]).


