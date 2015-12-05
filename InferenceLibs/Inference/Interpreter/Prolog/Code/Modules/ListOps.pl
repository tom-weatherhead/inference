% Module listOps - May 12, 2014
:- module(listOps, [length/2, member/2, append/3, addtoend/3, reverse/2, permutation/2, permutation2/2]).
%
length([], 0).
length([_ | T], N) :- length(T, M), N is M + 1.
%
member(X, [X | _]).
member(X, [_ | L]) :- member(X, L).
%
append([], L, L).
append([X | L1], L2, [X | L3]) :- append(L1, L2, L3).
%
addtoend([], X, [X]).
addtoend([H | T1], X, [H | T2]) :- addtoend(T1, X, T2).
%
reverse([], R, R).
reverse([X | L1], L2, R) :- reverse(L1, [X | L2], R).
reverse(L, R) :- reverse(L, [], R).
%
permutation([], []).
permutation(L, [H | T]) :- append(V, [H | U], L), append(V, U, W), permutation(W, T).
%
% Alternate permutation algorithm:
takeout(X, [X | R], R).
takeout(X, [F | R], [F | S]) :- takeout(X, R, S).
permutation2([], []).
permutation2([X | Y], Z) :- permutation2(Y, W), takeout(X, Z, W).