:- consult(frontiera_ord).
:- consult(search).

type float --> float+float.
type nodo(PN) --> nc(PN,list(PN),float).

%%%  STRATEGIA A* con taglio cicli

taglia_cicli(N,[S|L]) :- eq(N,S),!; taglia_cicli(N,L).

%%   USA FRONTIERA ORDINATA, implementata come priority queue
%%
%%   La frontiera importa dalla strategia l'ordinamento totale
%    leq(nodo(N),nodo(N))
%    leq(+N1, +N2):   N precede N2 nell'ordinamento della frontiera
%%

%%	h deve essere definita nel problema!!

leq(nc(N1,_,Costo1),nc(N2,_,Costo2)) :-
       h(N1, W1),
       h(N2, W2),
       Costo1+W1 =< Costo2+W2.
