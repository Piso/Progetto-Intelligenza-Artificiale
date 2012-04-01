% Seconda versione del progetto di Intelligenza artificiale
% Cambiamenti nella gestione del costo
% PROVA:usare 2 gruppi al posto di 1
% In questo modo abbiamo il costo, ma non vanno pi� bene i vicini!
:- use_module('types/chk').

:- no_check(setof(_,_,_)).

% classico tipo liste
type list(El) -->[];[El|list(El)].

type incrocio -->incrocio1;incrocio2;incrocio3;incrocio4;incrocio5;incrocio6;incrocio7;incrocio8;incrocio9;incrocio10.

% tipo strada->st(incrocio,incrocio collegato,peso della strada)
type strada--> st(incrocio,incrocio,int).

type mossa --> m(stato,stato,strada,strada).


% lo stato adesso non incorpora pi� la somma dei costi (provv.)
type stato -->in(incrocio,incrocio).

pred sicuro(incrocio).

pred agibile(strada).

pred costo(mossa,int).

% predicato per grafi NON ORIENTATI!!)
st(X,Y,P):-st(Y,X,P).


% il predicato trafficata non so se sia necessario
% potrebbe non essere necessario nel caso in cui utilizzassimo la
% definizione di agibile(strada):-incrocio destinaz. libero e non c'�
% pericolo.
pred trafficata(strada).


%Le prime mosse da considerare sono quelle in cui uno dei due gruppi
%e' al sicuro, queste mosse hanno priorit� pi� alta in quanto
%sono meno generali
%
m(in(Inc1,Inc2),in(Inc1,Inc3),_,Road):-sicuro(Inc1),Road=st(Inc2,Inc3,_),agibile(Road),!.
m(in(Inc1,Inc2),in(Inc3,Inc2),Road,_):-sicuro(Inc2),Road=st(Inc1,Inc3,_),agibile(Road),!.


m(in(Inc1,Inc2),in(Inc3,Inc4),Road1,Road2):-Road1=st(Inc1,Inc3,_),
	Road2=st(Inc2,Inc4,_),agibile(Road1),agibile(Road2).


costo(m(in(X,Y),in(Q,Z),st(X,Q,C1),st(Y,Z,C2)),C):- C is C1+C2.


pred trovato(stato).

trovato(in(Inc1)):-sicuro(Inc1).


% L � la lista dei "vicini di L"
% (incroci collegati ad L da una strada)
pred vicini(stato, list(stato)).

% sicuramente da ricontrollare i vicini qui sotto. Se necessario
% inserire anche gli altri vincoli (gruppi o constraint)
vicini(Stat,[]) :- trovato(Stat), !.
% sicuro che ci voglia il vicini sul trovato? Horni non l'ha mai
% fatto in classe,per� mi fido! ^^

vicini(Stat,L) :- setof(S1, m(Stat,S1,_),L), !. % ; L=[].
vicini(_Stat,[]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                %
%						 %
%      Esempio Semplice:10 Nodi 1 Gruppo	 %
%                                                %
%                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


in(incrocio1,0). %stato di partenza

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
