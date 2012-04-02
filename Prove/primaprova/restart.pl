% Versione modificata del progetto:
% cambio strada con connesso.
% per ora costo unitario.
% per ora pochi nodi.
%
:-use_module('types/chk').
:-consult(best_first).

type list(El) -->[]; [El|list(El)].
type incrocio --> inc1,inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9,inc10.
type stato -->in(incrocio,incrocio).
type real.


pred sicuro(incrocio).
pred trovato(stato).
pred connesso(incrocio,incrocio,int).
%pred connesso(incrocio,incrocio).
pred mossa(stato,stato).
pred vicini(stato,list(stato)).
pred agibile(incrocio,incrocio).
pred pericolo(incrocio).
pred eq(stato,stato).
connesso(X,Y,P):-connesso(Y,X,P).
%
agibile(X,Y):-connesso(X,Y,_),not(pericolo(Y)).
%mossa(in(X,Z),in(X,Q)):-sicuro(X),connesso(Z,Q,_),!.
%mossa(in(X,Z),in(Y,Z)):-sicuro(Z),connesso(X,Y,_),!.
%mossa(in(X,Z),in(Y,Q)):-connesso(X,Y,_),connesso(Z,Q,_).
%
mossa(in(X,Z),in(X,Q)):-sicuro(X),agibile(Z,Q),!.
mossa(in(X,Z),in(Y,Z)):-sicuro(Z),agibile(X,Y),!.
mossa(in(X,Z),in(Y,Q)):-agibile(X,Y),agibile(Z,Q).


trovato(in(Inc1,Inc2)):-sicuro(Inc1),sicuro(Inc2).

vicini(S,L):- setof(S1,mossa(S,S1),L),!.
vicini(_Stat,[]).

%costo(in(_,_),in(_,_),1).

costo(in(X,Z),in(X,Q),C):-connesso(Z,Q,C),!.
costo(in(X,Z),in(Y,Z),C):-connesso(X,Y,C),!.
costo(in(X,Z),in(Y,Q),C):-connesso(X,Y,C1),connesso(Z,Q,C2),C is C1 + C2.

eq(in(X,Y),in(Y,X)).

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







