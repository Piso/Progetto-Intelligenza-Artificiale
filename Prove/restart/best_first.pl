:- consult(frontiera_ord).
:- consult(search).

type float --> float+float.
type nodo(PN) --> nc(PN,list(PN),float).

%%%  STRATEGIA BEST FIRST con taglio cicli che usa eq

taglia_cicli(N,[S|L]) :- eq(N,S),!; taglia_cicli(N,L).

%%   USA FRONTIERA ORDINATA, implementata come priority queue
%%
%%   La frontiera importa dalla strategia l'ordinamento totale
%    leq(nodo(N),nodo(N))
%    leq(+N1, +N2):   N precede N2 nell'ordinamento della frontiera
%%

leq(nc(_,_,Costo1),nc(_,_,Costo2)) :-
       Costo1 =< Costo2.
