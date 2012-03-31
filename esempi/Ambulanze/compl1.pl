:- use_module(checker).

:- no_check([is(_,_)]).


%%%%%%%%  SONO DEFINITI DAL PROBLEMA
%type Nodo.
%type float.
%type list(X) --> [ ]  ;  [X | list(X)].


pred trovato(_Nodo).
   %  trovato(+N):    N è un goal, dipende dal problema
pred vicini(Nodo, list(Nodo)).
   % vicini(+N, -L):   L è la lista dei "vicini di L"
   %                   (collegati ad L da un arco)
pred costo(Nodo,Nodo,float).
   % costo(+N1,+N2,-C) : C è il costo dell'arco (N1,N2)


%%%%   SONO DEFINITI DALLA STRATEGIA


%type frontiera(_X).
%type list(X)--> [ ]  ;  [X | list(X)].

pred frontiera_iniziale(X,frontiera(X)).
   % frontiera_iniziale(+N,-F):   F è la frontiera con il solo
   %  nodo N.
pred scelto(X, frontiera(X), frontiera(X)).
  % scelto(-N, +F0,-F1): N è un nodo di F0 (il nodo selezionato)
  %			 e F1 è F0 senza N;
pred aggiunta(list(X), frontiera(X), frontiera(X)).
   % aggiunta(+L, +F1, -F2):      F2  si ottiene aggiungendo L ad F1


%%%%%%%%%%%%%%%%%%%%%%%%%%  ALGORITMO  %%%%%%%%%%%%

type float --> 0.
type list(X) --> [ ]  ;  [X | list(X)].
type frontiera(_X).
type stato(PN) --> stato(PN).
type nodo(PN) --> nc(PN,list(PN),float).


pred solve(_PN, nodo(_PN1)).
  % solve(+Start,nc(-Goal,-Path,-Cost)):   da Start si raggiunge
  %    Goal appraverso il cammino Path con costo Cost; Goal è una
  %    soluzione

pred cerca(frontiera(nodo(N)),nodo(N)).
  %  cerca(+F):      vi è un cammino da un nodo di F ad un goal
  %  inizialmente F=nodo iniziale, poi ricorsivamente altre frontiere

pred trasforma(list(N),nodo(N),list(nodo(N))).
   % trasforma(+Vicini,+N,-F):   F è la porzione di
   %        frontiera contenente i Vicini, da aggiungere
   %	    alla frontiera corrente


solve(N,G) :-
      frontiera_iniziale(nc(N,[],0),F0),
      cerca(F0,G).

cerca(Frontiera, nc(PN, Path, Cost)) :-
       scelto(nc(PN, Path, Cost),Frontiera,_),
       trovato(PN).		  % dal problema

cerca(Frontiera, Goal) :-
        scelto(nc(N,Path,Cost),Frontiera,F1),	 % dalla strategia
        vicini(N,Vicini),		 % dal problema
        trasforma(Vicini,nc(N,Path,Cost),FrontieraVicini),
        aggiunta(FrontieraVicini,F1,NuovaFrontiera),    % dalla strategia
        cerca(NuovaFrontiera,Goal).


%%  CON TAGLIO CICLI
trasforma([],nc(_,_,_),[]).
trasforma([V|T], nc(N,Path,Cost),TT) :-
        member(V,[N|Path]),!,
        trasforma(T,nc(N,Path,Cost),TT).
trasforma([V|T], nc(N,Path,Cost),[nc(V,[N|Path],Cost1)|TT]) :-
        costo(N,V,K),
	Cost1 is Cost+K,
        trasforma(T,nc(N,Path,Cost),TT).










