% Prima versione MODIFICATA del progetto di Intelligenza artificiale
%
:- use_module('types/chk').
:- no_check(setof(_,_,_)).
:- consult(depth).


type list(El) -->[];[El|list(El)].

type incrocio -->incrocio1;incrocio2;incrocio3;incrocio4;incrocio5;incrocio6;incrocio7;incrocio8;incrocio9;incrocio10.

type strada-->st(incrocio,incrocio,int).

%st(X,Y,P):-st(Y,X,P).

type nodo -->in(incrocio).

type mossa --> m(nodo,nodo,strada).

pred sicuro(incrocio).

pred agibile(strada).

pred trafficata(strada).

pred costo(nodo,nodo,int).

m(in(Inc1),in(Inc3),Road1):-Road1=st(Inc1,Inc3,_).%,agibile(Road1).
costo(in(X),in(Q),C):-st(X,Q,C1),C is C1.




pred trovato(nodo).

trovato(in(Inc1)):-sicuro(Inc1).


% L è la lista dei "vicini di L"
% (incroci collegati ad L da una strada)
pred vicini(nodo, list(stato)).

% sicuramente da ricontrollare i vicini qui sotto. Se necessario
% inserire anche gli altri vincoli (gruppi o constraint)
%vicini(Stat,[]) :- trovato(Stat), !.
% sicuro che ci voglia il vicini sul trovato? Horni non l'ha mai
% fatto in classe,però mi fido! ^^

vicini(Stat,L) :- setof(S1, m(Stat,S1,_),L), ! ; L=[].
%vicini(_Stat,[]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                %
%						 %
%      Esempio Semplice:10 Nodi 1 Gruppo	 %
%                                                %
%                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


in(incrocio1). %stato di partenza

sicuro(incrocio9).

st(incrocio1,incrocio2,5).
st(incrocio1,incrocio3,6).
st(incrocio1,incrocio4,1).
st(incrocio1,incrocio5,8).
st(incrocio2,incrocio5,4).
st(incrocio3,incrocio6,5).
st(incrocio4,incrocio3,8).
st(incrocio5,incrocio7,3).
st(incrocio6,incrocio7,2).
st(incrocio6,incrocio8,5).
st(incrocio7,incrocio10,3).
st(incrocio8,incrocio9,2). % nel pdf ho dimenticato il 2, will fix it
st(incrocio10,incrocio9,1).
