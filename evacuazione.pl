%Prima versione del progetto di Intelligenza artificiale
%
:- use_module('types/chk').

%classico tipo liste
type list(El) -->[];[El|list(El)].

%per ora ho pensato di mettere solo i nomi incrocio delegando a strada
%i collegamenti tra incroci e i pesi
%per ora ho messo solo 6 incroci. Aumentabile a piacere.

type incrocio -->incrocio1;incrocio2;incrocio3;incrocio4;incrocio5;incrocio6.

%tipo strada->st(incrocio,incrocio collegato,peso della strada)
type strada --> st(incrocio,incrocio,int).

%il tipo stato definisce la posizione sugli incroci di OGNI gruppo
%provvisorio: solo un gruppo!!
%FIXME: bisogna inserire un' info anche riguardante il numero
%del "turno di movimento"? forse serve x costo!!
%Si potrebbe calcolare direttamente il costo senza il turno sommando ad
%ogni turno il peso della strada percorsa.
%es: type stato -->in(incrocio,int). -> guarda mossa per la modifica
type stato -->in(incrocio).

pred sicuro(incrocio).

pred agibile(strada).

%il predicato trafficata non so se sia necessario
%potrebbe non essere necessario nel caso in cui utilizzassimo la
%definizione di agibile(strada):-incrocio destinaz. libero e non c'è
%pericolo.
pred trafficata(strada).

pred mossa(stato,stato,strada).

pred trovato(stato).

% L è la lista dei "vicini di L"
% (incroci collegati ad L da una strada)
pred vicini(stato, list(stato)).

%sicuramente da ricontrollare i vicini qui sotto. Se necessario
%inserire anche gli altri vincoli (gruppi o constraint)
vicini(Stat,[]) :- trovato(Stat), !.
vicini(Stat,L) :-  setof(S1, mossa(Stat,S1,_),L), !; L=[].



trovato(in(Inc1)):-sicuro(Inc1).

%Mossa non so se e' corretto (controllare unificazione road=
%sicuramente e' incompleto perchè mancano tutte le info relative a costo
%mossa(in(Inc1),in(Inc2),Road):- Road=st(Inc1,Inc2,_),agibile(Road).
%modifica con somma del costo:
mossa(in(Inc1,K),in(Inc2,K1),Road):-Road=st(Inc1,Inc2,P),agibile(Road),K1 is K+P.

