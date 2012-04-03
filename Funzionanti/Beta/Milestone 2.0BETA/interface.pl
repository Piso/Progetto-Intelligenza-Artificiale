:-  writeln('************************  Algoritmo generico ricerca ********\n').
:-  writeln('comandi:  load_problem(+F:nome_file), solve(+Start: nodo_problema, -Soluzione: nodo), show, noshow\n\n').

:- dynamic(showflag/0).

load_problem(F) :-
	consult(F).
show :-
	assert(showflag),
	writeln('.... entro in modalita'' interattiva'),
	sh.
sh :- writeln('comandi:   n: noshow;  a: abort; h: help').
noshow :-
	retractall(showflag).
command :-
	readln(R),
	(
	R = [], !
	;
	R = [h|_], !, sh, command
	;
	R = [n|_], !, noshow
	;
	R = [a|_], !, abort
        ;
	true
	).
