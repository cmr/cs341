% Paradigms
%
% In Procedural, operations are the "active" and data is "passive". That is,
% we overall care about the operations we do on the data. Additionally, the
% data is very memory-aware.
%
% In OO, the operations are secondary to the data. The data is the objects,
% and to do anything we send messages to them.
%
% In functional, we have: (data in) -> function -> (data out). That is, we
% write functions that just manipulate data given to them, but don't care
% about memory.
%
% In the Logic paradigm... we specify facts and rules, and can make queries on
% the "knowledge base" we've built up.
%
% (Goes through the prolog lab)
% male/female are predicates, and writing "male(abraham)" states the fact
% that abraham is a male.
%
% Same for mother/father, but they take two arguments.

male(abraham).
male(homer).
male(bartholomew).

female(mona).
female(lisa).
female(margaret).
female(marjorie).

mother(mona,homer).
mother(marjorie,bartholomew).
mother(marjorie,lisa).
mother(marjorie,margaret).

father(abraham,homer).
father(homer,bartholomew).
father(homer,lisa).
father(homer,margaret).

% Note: names starting with lowercase are constants, uppercase are variables.

% How do we define a sibling rule? Same father, and same mother.
% The comma in the rule body is conjunction (AND).

sibling(Person1, Person2) :-
    father(X, Person1), father(X, Person2),
    mother(Y, Person1), mother(Y, Person2).

% We are saying, "there has to be an X out there such that X is the father of
% Person1 and the father of Person2, but we don't care who X is. Same with
% mother. We can be less strict, and require same father OR same mother:

sibling2(Person1, Person2) :-
    father(X,Person1), father(X,Person2).
sibling2(Person1,Person2) :-
    mother(X,Person1), mother(X,Person2).

% Since we have two rules here for sibling2, the prolog interpreter will try
% each rule until one of them succeeds, in the same order as the source code.
% In this way, we have a disjunction (OR).
%
% The :- can be read as "if": "I consider Person1 a sibling of Person2 IF ..."
%
% We can exclude self-sibling:
sibling3(Person1,Person2) :-
    father(X,Person1), father(X,Person2),
    mother(Y,Person1), mother(Y,Person2),
    \+ Person1 = Person2.
% \+ is "not, so we add the condition that Person1 is not equal to Person2




% Lists in Prolog

% my_list(+Obj)
% determines if Obj is a list
my_list([]). % empty list is a list
my_list([X|Xs]) :- my_list(Xs).
    % X is the car, Xs is the cdr.
% X is not used at all. We can ignore it with an underscore.
% my_list([_|Xs]) :- my_list(Xs).

% my_length(+Ls, -Answer)
% This convention means "Ls is an input, Answer is an  output". But it's only
% for documentation.
%
% Determines the length of list Ls in Answer
my_length([], 0).
my_length([_|Xs], N) :-
    my_length(Xs, M),
    N is M+1.

% my_append(+Xs, +Ys, -Zs)
% compute append of Xs and Ys in Zs
my_append([], Ys, Ys).
my_append([X|Xs],Ys,[X|Zs]) :-
    my_append(Xs,Ys,Zs).

% my_reverse(+Xs,-Ys)
% compute inverse of list Xs in the list Ys
my_reverse([],[]).
my_reverse([X|Xs],Ys) :-
    my_reverse(Xs,As),
    my_append(As,[X],Ys).

% my_member(+Item, +Ls)
% Determines if Item exists in Ls
my_member(X, [X|Xs]).
my_member(X, [Y|Xs]) :- \+ X = Y, my_member(X,Xs).

% my_prefix(+Xs, +Ys)
% Is Xs a prefix of Ys?
my_prefix([], _).
my_prefix([X|Xs], [X|Ys]) :- my_prefix(Xs,Ys).

% my_insert(+Item, +SortedXs, -NewSortedXs)
my_insert(X, [], [X]).
my_insert(X, [Y|Ys], [X,Y|Ys]) :- X < Y.
my_insert(X, [Y|Ys], [Y|Zs]) :- \+ X < Y, my_insert(X, Ys, Zs).

% num_sort(+Xs, -Ys)
num_sort([], []).
num_sort([X|Xs], Ys) :-
    num_sort(Xs, As),
    my_insert(X, As, Ys).


factorial(0,1).
factorial(N,A) :-
    N > 0,
    M is N-1,
    factorial(M, PartialAnswer),
    A is N * PartialAnswer.
