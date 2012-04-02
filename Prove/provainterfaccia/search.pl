:- use_module('types/chk').

:- no_check(writeln(_)).

%%	TIPI PREDEFINITI

type float --> 0; 1; float + float; float-float; float/float; float * float.
type list(X) --> [ ]  ;  [X | list(X)].


%%%%%%%%  TIPO DEFINITO DAL PROBLEMA.

type stato.

%%%%   TIPO DEFINITO DALLA STRATEGIA

type frontiera.

%%    TIPO per la rappresentazione interna dei nodi

type nodo --> nc(stato,list(stato),float).


%%%%%%%% PREDICATI DEFINITI DAL PROBLEMA.

pred trovato(stato).
   %  trovato(+N) is semidet:    N e' un goal, dipende dal problema
pred vicini(stato, list(stato)).
   % vicini(+N, -L) is det:   L e' la lista dei "vicini di L"
   %                   (collegati ad L da un arco)
pred costo(stato, stato, float).
   % costo(+N1,+N2,-C) is det: C e' il costo dell'arco (N1,N2)

pred h(stato,float).
   % h(+N,-H) is det:  N e' il nodo corrente, H e' la stima
   % euristica del costo da N ad una soluzione ottimale


%%%%   PREDICATI DEFINITI DALLA STRATEGIA

pred frontiera_iniziale(nodo,frontiera).
   % frontiera_iniziale(+N,-F) is det:   F e' la frontiera con il solo
   %  nodo N.

pred scelto(nodo, frontiera, frontiera).
  % scelto(-N, +F0,-F1) is det: N e' un nodo di F0 (il nodo selezionato)
  %			 e F1 e' F0 senza N;
pred aggiunta(list(nodo), frontiera, frontiera).
   % aggiunta(+L, +F1, -F2) is det:      F2  si ottiene aggiungendo L ad F1

pred taglia_cicli(stato, list(stato)).
  %  taglia_cicli(S,L) is semidet:  vero se lo stato S o uno stato
  %     che include le soluzioni raggiungibili da S e' gia' stato
  %     incontrato in L (cammino dal nodo corrente alla radice)


%% PREDICATI DEFINITI DAL PROGRAMMA

pred solve(stato, nodo).
  % solve(+Start,nc(-Goal,-Path,-Cost)) is nondet:   da Start si raggiunge
  %    Goal appraverso il cammino Path con costo Cost; Goal e' una
  %    soluzione

pred cerca(frontiera,nodo).
  %  cerca(+F,-N) is nondet:      vi e' un cammino da un nodo di F ad un goal
  %  inizialmente F=nodo iniziale, poi ricorsivamente altre frontiere

pred trasforma(list(stato),nodo,list(nodo)).
   % trasforma(+Vicini,+N,-F) is det:   F e' la porzione di
   %        frontiera contenente i Vicini, da aggiungere
   %	    alla frontiera corrente

% ************************************************************************

:- consult(interfaccia).


solve(N,G) :-
      frontiera_iniziale(nc(N,[],0),F0),
      cerca(F0,G).

cerca(Frontiera, nc(PN, Path, Cost)) :-
       scelto(nc(PN, Path, Cost),Frontiera,_),
       trovato(PN),                      % dal problema
       (   showflag -> mostra(Frontiera),comandi; true).

cerca(Frontiera, Goal) :-
	(   showflag -> mostra(Frontiera),comandi; true),
        scelto(nc(N,Path,Cost),Frontiera,F1),	 % dalla strategia
        vicini(N,Vicini),		 % dal problema
        trasforma(Vicini,nc(N,Path,Cost),FrontieraVicini),
        aggiunta(FrontieraVicini,F1,NuovaFrontiera),    % dalla strategia
        cerca(NuovaFrontiera,Goal).



trasforma([],nc(_,_,_),[]).
trasforma([V|T], nc(N,Path,Cost),TT) :-
        taglia_cicli(V,[N|Path]),!,
        trasforma(T,nc(N,Path,Cost),TT).
trasforma([V|T], nc(N,Path,Cost),[nc(V,[N|Path],Cost1)|TT]) :-
        costo(N,V,K),
	Cost1 is Cost+K,
        trasforma(T,nc(N,Path,Cost),TT).














