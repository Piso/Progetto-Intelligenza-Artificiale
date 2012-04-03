:- use_module('types/chk').
:- consult(interface).

:- no_check(writeln(_)).

%%	TIPI PREDEFINITI

type float --> 0; 1; float + float; float-float; float/float; float * float.
type list(X) --> [ ]  ;  [X | list(X)].

%%%%   TIPO DEFINITO DALLA STRATEGIA
%      TPN  tipo nodi problema, aperto

type frontiera(_TNP).

%%    TIPO per la rappresentazione interna dei nodi

type nodo(TNP) --> nc(TNP,list(TNP),float).


%%%%%%%% PREDICATI DEFINITI DAL PROBLEMA.

pred trovato(_TNP).
   %  trovato(+N) is semidet:    N e' un goal, dipende dal problema
pred vicini(TNP, list(TNP)).
   % vicini(+N, -L) is det:   L e' la lista dei "vicini di L"
   %                   (collegati ad L da un arco)
pred costo(TNP, TNP, float).
   % costo(+N1,+N2,-C) is det: C e' il costo dell'arco (N1,N2)

pred h(_TNP,float).
   % h(+N,-H) is det:  N e' il nodo corrente, H e' la stima
   % euristica del costo da N ad una soluzione ottimale


%%%%   PREDICATI DEFINITI DALLA STRATEGIA

pred frontiera_iniziale(TNP,frontiera(TNP)).
   % frontiera_iniziale(+N,-F) is det:   F e' la frontiera con il solo
   %  nodo N.

pred scelto(TNP, frontiera(TNP), frontiera(TNP)).
  % scelto(-N, +F0,-F1) is det: N e' un nodo di F0 (il nodo selezionato)
  %			 e F1 e' F0 senza N;
pred aggiunta(list(TNP), frontiera(TNP), frontiera(TNP)).
   % aggiunta(+L, +F1, -F2) is det:      F2  si ottiene aggiungendo L ad F1

pred taglia_cicli(TNP, list(TNP)).
  %  taglia_cicli(S,L) is semidet:  vero se lo stato S o uno stato
  %     che include le soluzioni raggiungibili da S e' gia' stato
  %     incontrato in L (cammino dal nodo corrente alla radice)


%% PREDICATI DEFINITI DAL PROGRAMMA

%solve(TNP, nodo(TNP)).
  % solve(+Start,nc(-Goal,-Path,-Cost)) is nondet:   da Start si raggiunge
  %    Goal appraverso il cammino Path con costo Cost; Goal e' una
  %    soluzione

%cerca(frontiera(TNP),nodo(TNP)).
  %  cerca(+F,-N) is nondet:      vi e' un cammino da un nodo di F ad un goal
  %  inizialmente F=nodo iniziale, poi ricorsivamente altre frontiere

%trasforma(list(TNP),TNP,list(TNP)).
   % trasforma(+Vicini,+N,-F) is det:   F e' la porzione di
   %        frontiera contenente i Vicini, da aggiungere
   %	    alla frontiera corrente

% ************************************************************************




solve(N,G) :-
      frontiera_iniziale(nc(N,[],0),F0),
      cerca(F0,G).

cerca(Frontiera, nc(PN, Path, Cost)) :-
       scelto(nc(PN, Path, Cost),Frontiera,_),
       trovato(PN),                      % dal problema
       (   showflag -> mostra(Frontiera),command; true).

cerca(Frontiera, Goal) :-
	(   showflag -> mostra(Frontiera),command; true),
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














