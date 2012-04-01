:- use_module('types/chk').
:- consult('search').

type list(E) --> []; [E | list(E)].
type frontiera --> dl(list(nodo),list(nodo)).



taglia_cicli(_,_) :- fail.

frontiera_iniziale(N,dl([N|L],L)).

scelto(N,dl(U,V),dl(UU,V)) :-
	not(var(U)),
	U=[N|UU].


aggiunta(N,dl(F,G),dl(F,NF)) :-
	to_dif_list(N,dl(FN,LN)),
	concat_dl(dl(F,G), dl(FN,LN), dl(F,NF)).

to_dif_list([],dl(F,F)).
to_dif_list([X|R],dl([X|F],G)) :-
	to_dif_list(R,dl(F,G)).

concat_dl(dl(A,B),dl(B,C),dl(A,C)).

mostra(dl(L1,_)) :-
	var(L1), !.
mostra(dl([N|L1],L2)) :-
	writeln(N),
	mostra(dl(L1,L2)).






