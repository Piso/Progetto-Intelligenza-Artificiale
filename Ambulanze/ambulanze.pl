% Pianificazione del percorso ottimale di due ambulanze in ambito urbano
% Progetto di Intelligenza Artificiale I                           08/09
% 3 Febbraio 2009

% File principale del programma
% La soluzione al problema di ricerca ha inizio dal predicato:
% percorso(at(L1,L2,P1,A1,A2))
% •	L1: dove si trova la prima ambulanza
% •	L2: dove si trova la seconda ambulanza
% •	P1: lista dei pazienti ordinati per grado di emergenza
% •	A1: indica se la prima ambulanza ha o meno un paziente a bordo
% •	A2: indica se la seconda ambulanza ho o meno un paziente a bordo



:- use_module(checker).
:-use_module(library(time)).
:- consult(astar).

:- no_check([setof(_,_,_), is(_,_)]).


type int --> 0; 3; 6; 9.

% osp è l'ospedale, i sono gli isolati
type luogo --> osp; i2; i3; i4; i5; i6; i7.

type list(X) --> [ ]  ;  [X | list(X)].

% c se l'ambulanza ha un paziente a bordo, v altrimenti
type carica --> c;v.

% at(posizone ambulanza1,
%    posizione ambulanza2,
%    lista dei quartieri che contengono pazienti da curare,
%    situazione ambulanza1,
%    situazione ambulanza2)
type nodo --> at(luogo,luogo,list(luogo),carica,carica).


pred trovato(nodo).
   %  trovato(+S):    i due robo sono nei due lab
pred vicini(nodo, list(nodo)).
   % vicini(+N, -L):   L è la lista dei "vicini di L"
   %                   (collegati ad L da un arco)
pred costo(nodo,nodo, int).
   % costo(+N1,+N2,-C) : C è il costo dell'arco (N1,N2)

% goal: entrambe le ambulanze vuote sono rientrare all'ospedale
%       e non ci sono pazienti da curare
trovato(at(osp,osp,[],v,v)).

vicini(S,[]) :- trovato(S), !.
vicini(S,L) :-  setof(S1, mossa(S,S1),L), !; L=[].


% Possibili mosse per ogni singola ambulanza:
%      -M  si sposta
%      -C  carica un paziente a bordo
%      -S  scarica un paziente all'ospedale
%      -F  rimane ferma
%
% la prima lettera rappresenta l'azione della prima ambulanza
% la seconda lettera rappresenta l'azione della seconda ambulanza
%
% MM
costo(at(L1,L2,P1,A1,A2), at(N1,N2,P1,A1,A2), 3):-
       spostamento(L1,N1),
       spostamento(L2,N2),
       cond_mov(at(L1,L2,P1,A1,A2), D1),
       cond_mov(at(N1,N2,P1,A1,A2), D2),
       D2 < D1.

% CM
costo(at(L1,L2,[L1|P2],v,A2), at(L1,N2,P2,c,A2), 3):-
        spostamento(L2,N2),
       cond_mov(at(L1,L2,[L1|P2],v,A2), D1),
       cond_mov(at(L1,N2,P2,c,A2), D2),
       D2 < D1.

% SM
costo(at(osp,L2,P1,c,A2), at(osp,N2,P1,v,A2), 3):-
        spostamento(L2,N2),
       cond_mov(at(osp,L2,P1,c,A2), D1),
       cond_mov(at(osp,N2,P1,v,A2), D2),
       D2 < D1.

% MC
costo(at(L1,L2,[L2|P2],A1,v), at(N1,L2,P2,A1,c), 3):-
       spostamento(L1,N1),
       cond_mov(at(L1,L2,[L2|P2],A1,v), D1),
       cond_mov(at(N1,L2,P2,A1,c), D2),
       D2 < D1.

% CC
costo(at(L1,L2,[L1,L2|P2],v,v), at(L1,L2,P2,c,c), 3).

% SC
costo(at(osp,L2,[L2|P1],c,v), at(osp,L2,P1,v,c), 3).

% SS
costo(at(osp,osp,P1,c,c), at(osp,osp,P1,v,v), 3).

% MS
costo(at(L1,osp,P1,A1,c), at(N1,osp,P1,A1,v), 3):-
        spostamento(L1,N1),
       cond_mov(at(L1,osp,P1,A1,c), D1),
       cond_mov(at(N1,osp,P1,A1,v), D2),
       D2 < D1.

% CS
costo(at(L1,osp,[L1|P1],v,c), at(L1,osp,P1,c,v), 3).

% FM
costo(at(osp,L2,[],v,A2), at(osp,N2,[],v,A2), 3):-
       spostamento(L2,N2),
       cond_mov(at(osp,L2,[],v,A2), D1),
       cond_mov(at(osp,N2,[],v,A2), D2),
       D2 < D1.

% FC
costo(at(osp,L2,[L2],v,v), at(osp,L2,[],v,c), 3).

% FS
costo(at(osp,osp,[],v,c), at(osp,osp,[],v,v), 3).

% MF
costo(at(L1,osp,[],A1,v), at(N1,osp,[],A1,v), 3):-
       spostamento(L1,N1),
       cond_mov(at(L1,osp,[],A1,v), D1),
       cond_mov(at(N1,osp,[],A1,v), D2),
       D2 < D1.

% CF
costo(at(L1,osp,[L1],v,v), at(L1,osp,[],c,v), 3).

% SF
costo(at(osp,osp,[],c,v), at(osp,osp,[],v,v), 3).

mossa(S,S1) :- costo(S,S1,_).

% Grafo rappresentate il quartiere
strada(osp,i2).
strada(osp,i3).
strada(osp,i4).
strada(osp,i6).
strada(osp,i7).
strada(i2,i3).
strada(i3,i4).
strada(i5,i4).
strada(i4,i6).
strada(i7,i6).
strada(i7,i2).

% Strada bidirezionale
spostamento(X,Y) :- strada(X,Y); strada(Y,X).

%% Distanza minima da un punto all'altro
%% Utilizzata per la condizione di spostamento f
distanza(osp,i2,3).
distanza(osp,i3,3).
distanza(osp,i4,3).
distanza(osp,i6,3).
distanza(osp,i5,6).
distanza(osp,i7,3).
distanza(i2,i3,3).
distanza(i2,i7,3).
distanza(i2,i6,6).
distanza(i2,i4,6).
distanza(i2,i5,9).
distanza(i3,i4,3).
distanza(i3,i5,6).
distanza(i3,i6,6).
distanza(i3,i7,6).
distanza(i4,i5,3).
distanza(i4,i6,3).
distanza(i4,i7,6).
distanza(i5,i6,6).
distanza(i5,i7,9).
distanza(i6,i7,3).
distanza(XX,XX,0).

% Distanza bidirezionale
dist(X,Y,C) :- distanza(X,Y,C); distanza(Y,X,C).


%% CONDIZIONE MOVIMENTO (cond_mov)
%% cond_mov è usata per limitare le mosse possibili
%% cond_mov interviene solo nel caso in cui almeno un'ambulaza si muova
%% cond_mov impone che se un'ambulanza si muove allora si sta avvicinando alla soluzione
%% cond_mov è la distanza definita in questo modo:

% Ci sono più pazienti da caricare
% la prima è carica e la seconda scarica
cond_mov(at(L1,L2,[X|_],c,v), W):-  dist(L1,osp,W1), dist(L2,X,W2), dist(X,osp,W3), W is W1+W2+W3+3.

% Non ci sono pazienti da caricare e la lista è vuota
% la prima è carica e la seconda scarica
cond_mov(at(L1,L2,[],c,v), W):-
			  dist(L1,osp,W1),
			  dist(L2,osp,W2),
			  W is W1+W2.

% E' rimasto un solo paziente da caricare
% la prima è carica e la seconda scarica
cond_mov(at(L1,L2,[X],c,v), W):- dist(X,osp,W1),
			  dist(L1,osp,W2),
			  dist(L2,X,W3),
			  W is W1+W2+W3+3.

% Non ci sono pazienti da caricare e la lista è vuota
% la prima è scarica e la seconda carica
cond_mov(at(L1,L2,[],v,c), W):-  dist(L1,osp,W1),
			  dist(L2,osp,W2),
			  W is W1+W2.

% E' rimasto un solo paziente da caricare
% la prima è scarica e la seconda carica
cond_mov(at(L1,L2,[X],v,c), W):- dist(X,osp,W1),
			  dist(L1,X,W2),
			  dist(L2,osp,W3),
			  W is W1+W2+W3+3.

% Ci sono più pazienti da caricare
% la prima è scarica e la seconda carica
cond_mov(at(L1,L2,[X|_],v,c), W):- dist(L1,X,W1), dist(X,osp,W3), dist(L2,osp,W2), W is W1+W2+W3+3.

% Non ci sono più pazienti da caricare
% sono entrambe vuote
cond_mov(at(L1,L2,[],v,v), W):-  dist(L1,osp,W1),
			  dist(L2,osp,W2),
			  W is W1+W2.

% C'è un solo paziente da caricare
% sono entrambe vuote
% la prima è più vicina al paziente
cond_mov(at(L1,L2,[X],v,v), W):- dist(X,osp,W1),
			  dist(L1,X,W2),
			  dist(L2,X,W3),
			  W2 =< W3,
			  W is W1+W2+3.

% C'è un solo paziente da caricare
% sono entrambe vuote
% la seconda è più vicina al paziente
cond_mov(at(L1,L2,[X],v,v), W):- dist(X,osp,W1),
			  dist(L1,X,W2),
			  dist(L2,X,W3),
			  W3 < W2,
			  W is W1+W3+3.

% C'è più di un paziente da caricare
% la distanza della prima ambulanza dal primo paziente + quella
% della seconda ambulanza dal secondo paziente è minore dell'inverso,
% sono entrambe vuote
cond_mov(at(L1,L2,[X|C],v,v), W):- C=[Y|_],
			    dist(X,osp,W1),
			    dist(Y,osp,W2),
			  dist(L1,X,W3),
			  dist(L2,Y,W4),
			  dist(L1,Y,W5),
			  dist(L2,X,W6),
			  W3+W4 =< W5+W6,
			  W is W1+W2+W3+W4+3.

% C'è più di un paziente da caricare
% la distanza della prima ambulanza dal secondo paziente + quella
% della seconda ambulanza dal primo paziente è minore dell'inverso,
% sono entrambe vuote
cond_mov(at(L1,L2,[X|C],v,v), W):- C=[Y|_],
			    dist(X,osp,W1),
			    dist(Y,osp,W2),
			  dist(L1,X,W3),
			  dist(L2,Y,W4),
			  dist(L1,Y,W5),
			  dist(L2,X,W6),
			  W5+W6 < W3+W4,
			  W is W1+W2+W5+W6+3.

% Entrambe le ambulanze sono cariche
% e non ci sono più pazienti da caricare
cond_mov(at(L1,L2,[],c,c), W):-  dist(L1,osp,W1),
			  dist(L2,osp,W2),
			  W is W1+W2.

% Entrambe le ambulanze sono cariche
cond_mov(at(L1,L2,_,c,c), W):- dist(L1,osp,W1),dist(L2,osp,W2),W is W1+W2.


%%%%%%%%%%%%%%%%%% EURISTICA H %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilizzata in A*
%% H è l'intero W ottenuto moltiplicando 9 per l'arrotondamento
%% per eccesso della lunghezza della lista dei pazienti divisa per 2
h(at(_,_,P1,_,_), WE):- length(P1,D), WE is ceiling(D/2)*9.


%%%%%%%%%%%%%%%%%% STAMPA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
percorso(N):- get_time(TM1),
	      solve(N,nc(_,B,C)),
	      nl,
              write('TEMPO RISOLUZIONE EMERGENZA: '),write(C),
	      write(' minuti.'),
              nl,
	      nl,
	      writeln('Percorso: '),reverse(B,B1),stampa_lista(B1,0),
	      get_time(TM2),
	      TM is TM2-TM1,
              write('Tempo di elaborazione: '),write(TM),write(' secondi'),
	      statistics(globalused,MEM),
	      statistics(localused,LOC),
	      nl,
	      write('Stack globale: '),write(MEM),write(' byte'),
	      nl,
	      write('Stack locale: '),write(LOC),write(' byte').


stampa_lista([],N):- nl,write(N),write(': Tutti i pazienti sono in ospedale'), nl,nl.
stampa_lista([T|C],N):-nl,write(N),write(': '),write(T),N1 is N+3, stampa_lista(C,N1).











