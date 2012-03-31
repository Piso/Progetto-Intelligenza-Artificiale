%Prima versione del progetto di Intelligenza artificiale
%
:- use_module('types/chk').

%classico tipo liste
type list(El) -->[];[El|list(El)].

%per ora ho pensato di mettere solo i nomi incrocio delegando a strada
%i collegamenti tra incroci e i pesi
%per ora ho messo solo 6 incroci. Aumentabile a piacere.
%EDIT: incroci aumentati a 10 per l'esempio!!

type incrocio -->incrocio1;incrocio2;incrocio3;incrocio4;incrocio5;incrocio6;incrocio7;incrocio8;incrocio9;incrocio10.

%tipo strada->st(incrocio,incrocio collegato,peso della strada)
type strada-->st(incrocio,incrocio,int).

%predicato per grafi NON ORIENTATI!!)
st(X,Y,P):-st(Y,X,P).

%il tipo stato definisce la posizione sugli incroci di OGNI gruppo
%provvisorio: solo un gruppo!!
%FIXME: bisogna inserire un' info anche riguardante il numero
%del "turno di movimento"? forse serve x costo!!
%Si potrebbe calcolare direttamente il costo senza il turno sommando ad
%ogni turno il peso della strada percorsa.
%es: type stato -->in(incrocio,int). -> guarda mossa per la modifica
type stato -->in(incrocio,int).

pred sicuro(incrocio).

pred agibile(strada).

%il predicato trafficata non so se sia necessario
%potrebbe non essere necessario nel caso in cui utilizzassimo la
%definizione di agibile(strada):-incrocio destinaz. libero e non c'è
%pericolo.
pred trafficata(strada).

type mossa --> m(stato,stato,strada).

%Mossa non so se e' corretto (controllare unificazione road=
%mossa(in(Inc1),in(Inc2),Road):- Road=st(Inc1,Inc2,_),agibile(Road).
%modifica con somma del costo:
m(in(Inc1,K),in(Inc2,K1),Road):-Road=st(Inc1,Inc2,P),agibile(Road),K1 is K+P.

%nice, dovrebbe andare bene!


pred trovato(stato).

trovato(in(Inc1)):-sicuro(Inc1).


% L è la lista dei "vicini di L"
% (incroci collegati ad L da una strada)
pred vicini(stato, list(stato)).

%sicuramente da ricontrollare i vicini qui sotto. Se necessario
%inserire anche gli altri vincoli (gruppi o constraint)
vicini(Stat,[]) :- trovato(Stat), !.
%sicuro che ci voglia il vicini sul trovato? Horni non l'ha mai
%fatto in classe,però mi fido! ^^

vicini(Stat,L) :- setof(S1, m(Stat,S1,_),L), !. %&; L=[].
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
st(incrocio8,incrocio9,2). %nel pdf ho dimenticato il 2, will fix it
st(incrocio10,incrocio9,1).
