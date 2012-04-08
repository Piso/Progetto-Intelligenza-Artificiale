% Versione modificata del progetto:
% cambio strada con connesso.
% per ora costo unitario.
% per ora pochi nodi.
%
:-use_module('types/chk').
:-consult(best_first).

type list(El) -->[]; [El|list(El)].
type incrocio --> inc1,inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9,inc10.
type stato -->in(incrocio).
type nodo -->node(stato,stato).
type arco -->arc(nodo,nodo).
type real.


pred sicuro(incrocio).
pred trovato(nodo).
pred connesso(incrocio,incrocio,int).
%pred connesso(incrocio,incrocio).
pred mossa(stato,stato).
pred vicini(nodo,list(nodo)).
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
%mossa(in(X,Z),in(X,Q)):-sicuro(X),agibile(Z,Q),!.
%mossa(in(X,Z),in(Y,Z)):-sicuro(Z),agibile(X,Y),!.

arc(node(in(Inc1),in(Inc2)),node(in(Inc3),in(Inc4))):-
	connesso(Inc1,Inc3,_),
	connesso(Inc2,Inc4,_).

mossa(in(X),in(Y)):-agibile(X,Y).


trovato(node(in(Inc1),in(Inc2))):-sicuro(Inc1),sicuro(Inc2).

vicini(N,L):- setof(N1,arc(N,N1),L),!.
vicini(_Node,[]).

%costo(in(_,_),in(_,_),1).

%costi con aggiunta di traffico

%costi senza traffico
costo(in(X,Z),in(X,Q),C):-connesso(Z,Q,C),!.
costo(in(X,Z),in(Y,Z),C):-connesso(X,Y,C),!.
costo(in(X,Z),in(Y,Q),C):-connesso(X,Y,C1),connesso(Z,Q,C2),C is C1 + C2.

eq(node(in(X),in(Y)),node(in(X),in(Y))).


%Minimizzo il comando, non inserire punti dopo il nome dell'incrocio.
%
%
%%trovaSol(Z,Primo,Secondo):-solve(in(Primo,Secondo),Z).


sol(X,Y,Z):-solve(node(in(X),in(Y)),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sicuro(inc9).

pericolo(inc3).


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
connesso(inc9,inc8,2).
%connesso(inc10,inc9,1).



























































