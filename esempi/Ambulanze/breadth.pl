:- use_module(checker).
:- consult(compl1).




type list(X) --> []; [X|list(X)].
type frontiera(N) --> fl(list(N)).

pred frontiera_iniziale(X,frontiera(X)).
   % frontiera_iniziale(+N,-F):   F è la frontiera con il solo
   %  nodo N.
pred scelto(X, frontiera(X), frontiera(X)).
  % scelto(-N, +F0,-F1): N è un nodo di F0 (il nodo selezionato) 
  %			 e F1 è F0 senza N;
pred aggiunta(list(X), frontiera(X), frontiera(X)).
   % aggiunta(+L, +F1, -F2):      F2  si ottiene aggiungendo L ad F1



frontiera_iniziale(N,fl([N])).

scelto(N,fl([N|F]),fl(F)).

aggiunta(N,fl(F),fl(NF)) :- append(F,N,NF). 





