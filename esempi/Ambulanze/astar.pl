%%%  STRATEGIA A*

%%   USA FRONTIERA ORDINATA, implementata come priority queue 
%%   
%%   La frontiera importa dalla strategia l'ordinamento totale
%    leq(nodo(N),nodo(N))
%    leq(+N1, +N2):   N precede N2 nell'ordinamento della frontiera
%%   

:- use_module(checker).
:- consult(compl1).
:- consult(frontiera_ordinata).


type float.
type nodo(PN) --> nc(PN,list(PN),float).

leq(nc(N1,_,Costo1),nc(N2,_,Costo2)) :-
      h(N1, W1),
      h(N2, W2),
       Costo1+W1 =< Costo2+W2. 
