:- use_module('types/chk').
:- consult(search).

:- no_check(maplist(_,_)).

load_problem(F) :-
	consult(F).

type frontiera --> fl(list(nodo)).
type list(X) --> []; [X|list(X)].

%taglia_cicli(_,_) :- fail.
taglia_cicli(N,P) :- member(N,P).

frontiera_iniziale(N,fl([N])).

scelto(N,fl([N|F]),fl(F)).

aggiunta(N,fl(F),fl(NF)) :- append(N,F,NF).

mostra(fl(F)) :-
	maplist(writeln,F).

