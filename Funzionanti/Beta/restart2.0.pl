% Versione modificata del progetto:
% cambio strada con connesso.
% per ora costo unitario.
% per ora pochi nodi.
%
:-use_module('types/chk').
:-consult(depth).

type list(El) --> [El|list(El)].
type incrocio --> inc1,inc2,inc3,inc4.
type stato -->in(incrocio).
type real.


pred sicuro(incrocio).
pred trovato(stato).
pred connesso(incrocio,incrocio,int).
%pred connesso(incrocio,incrocio).
pred mossa(stato,stato).
pred vicini(stato,list(stato)).

%%connesso(X,Y):-connesso(Y,X).
mossa(in(X),in(Y)):-connesso(X,Y,_).
trovato(in(Inc1)):-sicuro(Inc1).

vicini(S,L):- setof(S1,mossa(S,S1),L),!.
vicini(_Stat,[]).

%costo(in(_),in(_),1).
costo(in(X),in(Y),C):-connesso(X,Y,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sicuro(inc4).

connesso(inc1,inc2,2).
connesso(inc1,inc5,1).
connesso(inc5,inc4,1).
connesso(inc2,inc3,2).
connesso(inc3,inc4,2).
