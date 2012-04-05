:-use_module('types/chk').
:-consult(best_first).

type list(El) --> []; [El|list(El)].
type incrocio --> inc1,inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9,inc10.
%type stato --> in(incrocio,incrocio).
type stato --> in(incrocio,int).
type clima --> sole,pioggia,tempesta,neve.
type mezzo --> auto,tram,bus,piedi.
type real.


%pred folla(mossa). fare con calma.
%pred occupato(incrocio). decommentare e pensare come far attendere
%un gruppo.
pred sicuro(incrocio).
pred trovato(stato).
pred connesso(incrocio,incrocio,int).
%pred connesso(incrocio,incrocio).
pred mossa(stato,stato).
pred vicini(stato,list(stato)).
pred agibile(incrocio,incrocio).
pred pericolo(incrocio).
pred traffico(incrocio,incrocio).
%pred dissestato(incrocio,incrocio). facciamo? problema da discutere.
pred meteo(clima,int).
pred equip(mezzo,int).
%pred incendio(incrocio).
% se un incrocio � connesso a questo il peso della strada che lo collega
% aumenta per via dell'incendio in corso.
pred arco --> ([stato,stato],[stato,stato],[mossa,mossa]).

arco((in(S1,_),in(S3,_)),(in(S2,_),in(S4,_),mossa(in(S1,_),in(S2,_)),mossa2(in(S3,_),in(S4,_))):-
				   strada(S1,S2,mossa1),
				   strada(S3,S4,mossa2).


%connesso(X,Y,P):-connesso(Y,X,P).

agibile(X,Y):-
	connesso(X,Y,_),
	not(pericolo(Y)).


mossa(in(X,Z),in(X,Q)):-
	sicuro(X),
	agibile(Z,Q),!.

mossa(in(X,Z),in(Y,Z)):-
	sicuro(Z),
	agibile(X,Y),!.

mossa(in(X,Z),in(Y,Q)):-
	agibile(X,Y),
	agibile(Z,Q).

%Flusso, la folla si crea mentre entrambi i gruppi si spostano.
%folla(in(X,Y),in(Y,Q)):-
	%agibile(X,Y),
	%agibile(Y,Q),
	%X \= Q.
%Folla sotto creata dal movimento di un gruppo in nodo occupato.
%folla(in(X,Y),in(Y,Y)):-
%	 agibile(X,Y),
%
%mossa() :-agibile,folla...in progress...

%Modifico trovato per funzionare con 2 gruppi indipendenti
%trovato(in(Inc1,Inc2)):-
	%sicuro(Inc1),
	%sicuro(Inc2).
trovato(in(inc1,_)):-sicuro(inc1).

vicini(S,L):-
	setof(S1,mossa(S,S1),L),!.
vicini(_Stat,[]).

%costo(in(_,_),in(_,_),1).

%costi con aggiunta di traffico

costo(in(X,Z),in(X,Q),C):-
	traffico(Z,Q),
	connesso(Z,Q,C1),
	meteo(_,P),
	equip(_,P1),
	C is C1 + 5 + P + P1,!.

costo(in(X,Z),in(Y,Z),C):-
	traffico(X,Y),
	connesso(X,Y,C1),
	meteo(_,P),
	equip(_,P1),
	C is C1 + 5 + P + P1,!.

%Qui sotto traffico su entrambe le strade.
costo(in(X,Z),in(Y,Q),C):-
	traffico(X,Y),
	traffico(Z,Q),
	connesso(X,Y,C1),
	connesso(Z,Q,C2),
	meteo(_,P),
	equip(_,P1),
	C is C1 + C2 + 10 + P + P1,!.

%Qui sotto traffico su una delle due strade.
costo(in(X,Z),in(Y,Q),C):-
	(traffico(X,Y);traffico(Z,Q)),
	connesso(X,Y,C1),
	connesso(Z,Q,C2),
	meteo(_,P),
	equip(_,P1),
	C is C1 + C2 + 5 + P + P + P1 + P1,!.

%costi senza traffico
%muove solo gruppo 2
costo(in(X,Z),in(X,Q),C):-
	connesso(Z,Q,C1),
	meteo(_,P),
	equip(_,P1),
	C is C1 + P + P1,!.

%muove solo gruppo 1
costo(in(X,Z),in(Y,Z),C):-
	connesso(X,Y,C1),
	meteo(_,P),
	equip(_,P1),
	C is C1 + P + P1,!.

%muovono entrambi i gruppi
costo(in(X,Z),in(Y,Q),C):-
	connesso(X,Y,C1),
	meteo(_,P),
	connesso(Z,Q,C2),
	equip(_,P1),
	C is C1 + C2 + P + P + P1 + P1.

eq(in(X,Y),in(X,Y)).


%Minimizzo il comando

risolvi(Z):-
	write('Stato primo gruppo: '),
	read(X),
	write('Stato secondo gruppo: '),
	read(Y),
	(sol(X,Y,Z)).
%trovaSol(Z,Primo,Secondo):-solve(in(Primo,Secondo),Z).


sol(X,Y,Z):-solve(in(X,Y),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%meteo(sole,0).
%meteo(pioggia,7).
%meteo(tempesta,12).
meteo(neve,10).

%equip(auto,0).
%equip(tram,3).
%equip(bus,2).
equip(piedi,5).

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



























































