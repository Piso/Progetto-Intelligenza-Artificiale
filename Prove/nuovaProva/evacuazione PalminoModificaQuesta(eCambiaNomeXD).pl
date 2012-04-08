% Versione modificata del progetto:
% cambio strada con connesso.
% per ora costo unitario.
% per ora pochi nodi.
%
:-use_module('types/chk').
:-consult(best_first).

type list(El) -->[]; [El|list(El)].
type incrocio --> inc1;inc2;inc3;inc4;inc5;inc6;inc7;inc8;inc9;inc10.
type stato -->in(situazione,situazione).
type situazione --> arrivato(incrocio);viaggio(incrocio,int).
type real.


pred sicuro(incrocio).
pred trovato(stato).
pred connesso(incrocio,incrocio,int).
%pred connesso(incrocio,incrocio).
pred mossa(stato,stato).
pred vicini(stato,list(stato)).
pred agibile(incrocio,incrocio).
pred pericolo(incrocio).
pred traffico(incrocio,incrocio).

%connesso(X,Y,P):-connesso(Y,X,P).
%
agibile(X,Y):-connesso(X,Y,_),not(pericolo(Y)).
%mossa(in(X,Z),in(X,Q)):-sicuro(X),connesso(Z,Q,_),!.
%mossa(in(X,Z),in(Y,Z)):-sicuro(Z),connesso(X,Y,_),!.
%mossa(in(X,Z),in(Y,Q)):-connesso(X,Y,_),connesso(Z,Q,_).
%
%

%primo gruppo arrivato al sicuro, secondo gruppo in viaggio

mossa(in(arrivato(X),viaggio(Q,_)),in(arrivato(X),arrivato(Q))):-
	sicuro(X),!.

%secondo gruppo arrivato al sicuro, primo gruppo in viaggio

mossa(in(viaggio(X,_),arrivato(Q)),in(arrivato(X),arrivato(Q))):-
	sicuro(Q),!.

%primo gruppo al sicuro, secondo in incrocio

mossa(in(arrivato(X),arrivato(Q)),in(arrivato(X),arrivato(Z))):-
	connesso(Q,Z,_),
	sicuro(X),!.

%secondo gruppo al sicuro,primo in incrocio

mossa(in(arrivato(X),arrivato(Q)),in(arrivato(Y),arrivato(Q))):-
	connesso(X,Y,_),
	sicuro(Q),!.

%In viaggio il primo gruppo, arriva primo gruppo
%

mossa(in(viaggio(X,N),arrivato(Z)),in(arrivato(X),viaggio(Q,N2))):-
	connesso(Z,Q,L),
	L > N,
	N2 is L-N,!.

%In viaggio il primo, arriva il secondo
mossa(in(viaggio(X,N),arrivato(Z)),in(viaggio(X,N2),arrivato(Q))):-
	connesso(Z,Q,L),
	N > L,
	N2 is N-L,!.

%In viaggio il secondo, arriva il primo
mossa(in(arrivato(X),viaggio(Q,N)),in(arrivato(Y),viaggio(Q,N2))):-
	connesso(X,Y,L),
	N > L,
	N2 is N-L,!.

%In viaggio il secondo, arriva il secondo
mossa(in(arrivato(X),viaggio(Q,N)),in(viaggio(Y,N2),arrivato(Q))):-
	connesso(X,Y,L),
	L > N,
	N2 is L - N,!.


%Partono entrambi da incrocio, il primo arriva a dest.
mossa(in(arrivato(X),arrivato(Z)),in(arrivato(Y),viaggio(Q,N))):-
      connesso(X,Y,L),
      connesso(Z,Q,L2),
      L2 > L,
      N is L2 - L,!.

%Partono entrambi da incrocio, il secondo arriva a dest.
mossa(in(arrivato(X),arrivato(Z)),in(viaggio(Y,N),arrivato(Q))):-
	connesso(X,Y,L),
	connesso(Z,Q,L2),
	L > L2,
	N is L-L2,!.


%mossa(in(X,Z),in(X,Q)):-sicuro(X),agibile(Z,Q),!.
%mossa(in(X,Z),in(Y,Z)):-sicuro(Z),agibile(X,Y),!.
%mossa(in(X,Z),in(Y,Q)):-agibile(X,Y),agibile(Z,Q).


trovato(in(arrivato(Inc1),arrivato(Inc2))):-sicuro(Inc1),sicuro(Inc2).

vicini(S,L):- setof(S1,mossa(S,S1),L),!.
vicini(_Stat,[]).

costo(in(_,_),in(_,_),1).


%costi senza traffico
%costo(in(X,Z),in(X,Q),C):-connesso(Z,Q,C),!.
%costo(in(X,Z),in(Y,Z),C):-connesso(X,Y,C),!.
% costo(in(X,Z),in(Y,Q),C):-connesso(X,Y,C1),connesso(Z,Q,C2),C is C1 +
% C2.

eq(in(X,Y),in(X,Y)).


%Minimizzo il comando, non inserire punti dopo il nome dell'incrocio.
%
%
%


risolvi(Z):-write('Stato primo gruppo: '),read(X),
	    write('Stato secondo gruppo: '),read(Y),
	    (sol(X,Y,Z)).
%trovaSol(Z,Primo,Secondo):-solve(in(Primo,Secondo),Z).


sol(X,Y,Z):-solve(in(arrivato(X),arrivato(Y)),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sicuro(inc9).

pericolo(inc3).

traffico(inc1,inc2).
traffico(inc4,inc1).
traffico(inc1,inc5).

connesso(inc1,inc2,5).
connesso(inc1,inc5,8).
connesso(inc1,inc3,6).
connesso(inc1,inc4,1).
connesso(inc2,inc5,4).
connesso(inc3,inc6,5).
connesso(inc4,inc3,8).
connesso(inc5,inc7,3).
connesso(inc6,inc7,2).
connesso(inc6,inc8,5).
connesso(inc7,inc10,3).
connesso(inc8,inc9,2).
connesso(inc10,inc9,1).


connesso(inc2,inc1,5).
connesso(inc5,inc1,8).
connesso(inc3,inc1,6).
connesso(inc4,inc1,1).
connesso(inc5,inc2,4).
connesso(inc6,inc3,5).
connesso(inc3,inc4,8).
connesso(inc7,inc5,3).
connesso(inc7,inc6,2).
connesso(inc8,inc6,5).
connesso(inc10,inc7,3).
%connesso(inc9,inc8,2).
%connesso(inc10,inc9,1).


























































