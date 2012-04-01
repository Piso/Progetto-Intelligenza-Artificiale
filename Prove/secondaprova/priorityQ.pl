/*:- module(priorityQ, [ins/4,
		      get/4]).*/

:- use_module('types/chk').

:- no_check(once(_)).

type alb(E) --> o; a(E,alb(E),alb(E)).




%%%%   IMPLEMENTAZIONE: alberi di priorità sono alberi
%%%%   binari con le seguenti proprieta' invarianti:
%
% Proprieta' di priorita':   la radice di ogni albero e sottoalbero
%         ne e' il minimo
%
% Proprieta' di bilanciamento:  per ogni albero e sottoalbero,
% detti ND ed NS i numeri dei nodi dei sottoalberi destro e sinistro,
% vale:    ND = NS  o ND = NS+1
%
% Il bilanciamento garantisce che se T e' il numero totale
% dei nodi di un albero, i cammini hanno lunghezza
% L <= log_2(T)+1





% ins(+N:X,+F:alb(X),-F1:alb(X)):
%   pre:  F albero di priorità
%   post:  F1 = F unito X e F1 albero di priorità

ins(X, o, a(X,o,o), _Leq):- !.
ins(X, a(N,S,D), a(N1,D,S1), Leq) :-
   call(Leq, X,N) ->
       (  N1 = X,
          ins(N, S, S1,Leq)
        )
       ;
       (  N1 = N,
          ins(X, S, S1,Leq)).

%%%%%%%%%      get(-N:Nodo, +F:alb(Nodo),-F1:alb(Nodo))
%              N = nodo minimo di F (con ordinamento leq)
%              e F1 = F meno N
%              e  se F e' di priorita' lo e' anche F1

extract(X, a(X,o,o), o, _) :- !.
extract(N, a(N,S,D), a(N1,S1,D1), Leq) :-
   once(ext_foglia(F, D, DmenoF)),
   once(riempi(F, a(_,DmenoF,S), a(N1,S1,D1),Leq)).


%%% ext_foglia(-Foglia:X, +F:alb(X), -F1:alb(X))
%%            Foglia e' una foglia di F,
%%	      F1 = F meno Foglia,
%%	      se F è di priorità, lo è anche F1
%%            Tempo O(log(numNodi(F)))

ext_foglia(F, a(F,o,o), o) :- !.
ext_foglia(F, a(N,S,D), a(N,ND,S)) :-
    ext_foglia(F,D,ND).

%%% riempi(+N:X, +F:alb(X), -F1:alb(X))
%%            N e' un nodo arbitrario,
%%            F e' un albero da cui la radice e' stata tolta;
%%                diremo che F ha un "buco", che indicheremo con _ :
%%		  F = a(_, S, D)
%%	      F1 riempie il buco _ di F con N, portandolo al posto
%%                giusto per mantenere l'ordinamento
%%            Tempo O(log(numNodi(F)))


% caso base1:  F = a(_,o,o), cioe' F contiene solo il buco _,
%              che viene riempito con il nodo N

riempi(N, a(_,o,o), a(N,o,o),_).

% caso base 2:   F = a(_,o,a(ND,o,o)), cioe' F contiene il buco e sola
%                una foglia, a destra per il bilanciamento:
%                se N <= ND, riempie il buco,
%                altrimenti lo riempie ND e N va al posto di ND

riempi(N, a(_,o,a(ND,o,o)),a(N, o, a(ND,o,o)),Leq) :-
     call(Leq,N,ND), !.
riempi(N, a(_,o,a(ND,o,o)),a(ND, o, a(N,o,o)),_).


% caso ricorsivo 1:  due sottoalberi con radici N1,N2
%                    e N <= di entrambe:   mettiamo N nel
%                    buco senza chiamata ricorsiva alcuna
riempi(N, a(_,a(N1,S1,D1),
                a(N2,S2,D2)),
	    a(N,a(N1,S1,D1),
                a(N2,S2,D2)), Leq) :-
     call(Leq,N,N1),
     call(Leq,N,N2), !.

% caso ricorsivo 2:  due sottoalberi con radici N1,N2
%                    e, per il cut!,  N > di almeno uno fra N1,N2
%		     e, per il test leq(N1,N2), N1 e' il minimo
%		     fra N, N1, N2:  mettiamo N1 nel buco, aprendo
%                    un buco nel sotto-albero di N1;
%                    Ricorsivamente, riempiamo il buco con N in tale
%                    sottoalbero

riempi(N, a(_,a(N1,S1,D1),
                a(N2,S2,D2)),
	    a(N1,RiempitoS,
                a(N2,S2,D2)), Leq) :-
     call(Leq,N1,N2), !,
     riempi(N, a(_, S1,D1), RiempitoS,Leq).

% caso ricorsivo 3:  due sottoalberi con radici N1,N2
%                    e, per i due cut!,  N2 e' il minimo
%		     fra N, N1, N2:  mettiamo N2 nel buco, aprendo
%                    un buco nel sotto-albero di N2;
%                    Ricorsivamente, riempiamo il buco con N in tale
%                    sottoalbero

riempi(N, a(_,a(N1,S1,D1),
                a(N2,S2,D2)),
	    a(N2,a(N1,S1,D1),
                 RiempitoD),Leq) :-
     riempi(N, a(_, S2,D2), RiempitoD,Leq).


