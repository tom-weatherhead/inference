% Module prettyPrint - May 12, 2014
:- module(prettyPrint, [prettyPrint/1]).
length([], 0).
length([_ | T], N) :- length(T, M), N is M + 1.
zeroOrMoreSpaces(N) :- tab(N), !.
zeroOrMoreSpaces(N) :- N =:= 0.
writeFunctor(Indent, H) :- zeroOrMoreSpaces(Indent), write(H).
writeOpenBracket([]).
writeOpenBracket([_ | _]) :- write('('), nl.
writeArgs(_, [], _, _).
writeArgs(Indent, [Arg | ArgT], ArgNum, NumArgs) :-
    prettyPrint(Arg, Indent, ArgNum, NumArgs),
    NextArgNum is ArgNum + 1,
    writeArgs(Indent, ArgT, NextArgNum, NumArgs).
writeCloseBracket(_, []).
writeCloseBracket(Indent, [_ | _]) :- zeroOrMoreSpaces(Indent), write(')').
writeComma(ArgNum, NumArgs) :- ArgNum < NumArgs, !, write(',').
writeComma(_, _).
prettyPrint(X, Indent, ArgNum, NumArgs) :-
    X =.. Y,
    Y = [H | T],
    length(T, LengthT),
    writeFunctor(Indent, H),
    writeOpenBracket(T),
    NextIndent is Indent + 4,
    writeArgs(NextIndent, T, 1, LengthT),
    writeCloseBracket(Indent, T),
    writeComma(ArgNum, NumArgs),
    nl.
prettyPrint(X) :- prettyPrint(X, 0, 1, 1).