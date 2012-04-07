% Versione modificata del progetto:
% cambio strada con connesso.
% per ora costo unitario.
% per ora pochi nodi.
%
:-use_module('types/chk').
:-consult(best_first).

type list(El) --> []; [El|list(El)].
type incrocio --> inc1,inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9,inc10.
%type stato --> in(incrocio,int). %luogo e tempo
type real.
%type nodo -->node(stato,stato).
type nodo -->node(incrocio,incrocio).
type arco -->arc(nodo,nodo,mossa,mossa).

pred sicuro(incrocio).
pred trovato(node).
pred connesso(incrocio,incrocio,int).
pred mossa(stato,stato).
pred vicini(nodo,list(nodo)).
%pred arc(nodo,nodo,[mossa,mossa]).
%pred arc1(stato,stato,mossa).
%pred node(stato,stato).



%connesso(X,Y,P):-connesso(Y,X,P).
%
%arc([S1,S2],[S3,S4],[M1,M2]):-
%	arc1(S1,S3,M1),
%	arc1(S2,S4,M2).

%arc(node(S1,S2),node(S3,S4),[M1,M2]):-
%	arc1(S1,S3,M1),
%	arc1(S2,S4,M2).


arc(node(Inc1,Inc2),node(Inc3,Inc4),M1,M2):-
	M1=mossa(Inc1,Inc3),
	M2=mossa(Inc2,Inc4).

arc1(S1,S2,mossa(S1,S2)).



mossa(Inc1,Inc2):-connesso(Inc1,Inc2,_).


%trovato(node(in(Inc1,_),in(Inc2,_))):-
%	sicuro(Inc1),
%	sicuro(Inc2).

trovato(node(Inc1,Inc2)):-
	sicuro(Inc1),
	sicuro(Inc2).

%vicini(S,L):-
%	setof(S1,mossa(S,S1),L),!.
%vicini(_Stat,[]).


%vicini(node(S1,S2),L):-
%setof(node(S3,S4),arc(node(S1,S2),node(S3,S4),[mossa(S1,S3),mossa(S2,S4)])
      %,L),!.
%vicini(_Node,[]).

vicini(N,L):-
	setof(N1,arc(N,N1,_,_),L).
vicini(_Node,[]).

%costo(in(_,_),in(_,_),1).

%costi con aggiunta di traffico



eq(node(X,Y),node(X,Y)).




%sol(X,Y,Z):-solve(node(in(X,0),in(Y,0)),Z).

sol(X,Y,Z):-solve(node(X,Y),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sicuro(inc9).

pericolo(inc3).

traffico(inc9,inc10).
%traffico(inc4,inc1).
%traffico(inc1,inc5).

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



























































