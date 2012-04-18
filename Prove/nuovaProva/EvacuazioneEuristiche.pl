:-use_module('types/chk').
:-consult(astar).

type list(El) -->[]; [El|list(El)].
type incrocio --> inc1;inc2;inc3;inc4;inc5;inc6;inc7;inc8;inc9;inc10.
type stato -->in(situazione,situazione).
type situazione --> arrivato(incrocio);viaggio(incrocio,int).
type real.


pred sicuro(incrocio).
pred trovato(stato).
pred costo(stato,stato,int).
pred connesso(incrocio,incrocio,int).
pred mossa(stato,stato).
pred vicini(stato,list(stato)).

pred distMan(incrocio,int).
pred distEuc(incrocio,int).


pred agibile(incrocio,incrocio).
pred pericolo(incrocio).
pred traffico(incrocio,incrocio).


agibile(X,Y):-connesso(X,Y,_),not(pericolo(Y)).

pred coefficiente1(int). %coefficiente dato dai costraint al gruppo 1
pred coefficiente2(int).
pred coefficienteMeteo(int).

%primo gruppo arrivato al sicuro, secondo gruppo in viaggio

mossa(in(arrivato(X),viaggio(Q,_)),in(arrivato(X),arrivato(Q))):-
	sicuro(X).

%secondo gruppo arrivato al sicuro, primo gruppo in viaggio

mossa(in(viaggio(X,_),arrivato(Q)),in(arrivato(X),arrivato(Q))):-
	sicuro(Q).

%primo gruppo al sicuro, secondo in incrocio

mossa(in(arrivato(X),arrivato(Q)),in(arrivato(X),arrivato(Z))):-
	connesso(Q,Z,_),
	sicuro(X).

%secondo gruppo al sicuro,primo in incrocio

mossa(in(arrivato(X),arrivato(Q)),in(arrivato(Y),arrivato(Q))):-
	connesso(X,Y,_),
	sicuro(Q).

mossa(in(arrivato(X),arrivato(X)),in(arrivato(Y),arrivato(Y))):-
	connesso(X,Y,_).

%In viaggio il primo gruppo, arriva primo gruppo

mossa(in(viaggio(X,N),arrivato(Z)),in(arrivato(X),viaggio(Q,N2))):-
	connesso(Z,Q,L),
	L > N,
	N2 is L-N.

%In viaggio il primo, arriva il secondo
mossa(in(viaggio(X,N),arrivato(Z)),in(viaggio(X,N2),arrivato(Q))):-
	connesso(Z,Q,L),
	N > L,
	N2 is N-L.

%in viaggio primo, secondo da inc, arrivano entrambi
mossa(in(viaggio(X,N),arrivato(Z)),in(arrivato(X),arrivato(Q))):-
	connesso(Z,Q,L),
	N=:=L,!.

%Primo parte da incrocio,In viaggio il secondo, arriva il primo
mossa(in(arrivato(X),viaggio(Q,N)),in(arrivato(Y),viaggio(Q,N2))):-
	connesso(X,Y,L),
	N > L,
	N2 is N-L.

%primo parte da incrocio,In viaggio il secondo, arriva il secondo
mossa(in(arrivato(X),viaggio(Q,N)),in(viaggio(Y,N2),arrivato(Q))):-
	connesso(X,Y,L),
	L > N,
	N2 is L - N.

%Primo parte da incrocio, secondo in viaggio, arrivano entrambi
mossa(in(arrivato(X),viaggio(Q,L)),in(arrivato(Y),arrivato(Q))):-
	connesso(X,Y,N),
	sicuro(Y),
	L=:=N,!.


%Partono entrambi da incrocio, il secondo arriva a dest.
mossa(in(arrivato(X),arrivato(Z)),in(viaggio(Y,N),arrivato(Q))):-
	connesso(X,Y,L),
	connesso(Z,Q,L2),
	L > L2,
	N is L-L2.

%Partono entrambi da incrocio, il primo arriva a dest.
mossa(in(arrivato(X),arrivato(Z)),in(arrivato(Y),viaggio(Q,N))):-
      connesso(X,Y,L),
      connesso(Z,Q,L2),
      L2 > L,
      N is L2 - L.

%Partono entrambi da incrocio, arrivano entrambi in un incrocio
mossa(in(arrrivato(X),arrivato(Y)),in(arrivato(Z),arrivato(Q))):-
	connesso(X,Z,L1),
	connesso(Y,Q,L2),
	L1=:=L2,!.



trovato(in(arrivato(Inc1),arrivato(Inc2))):-sicuro(Inc1),sicuro(Inc2).

vicini(S,L):- setof(S1,mossa(S,S1),L),!.
vicini(_Stat,[]).


%Definito costo unitario su tutte le strade
%costo(in(_,_),in(_,_),1).

%primo al sicuro,secondo in incrocio
costo(in(arrivato(X),arrivato(Y)),in(arrivato(X),arrivato(Q)),C):-
	connesso(Y,Q,C2),
	C is C2,!.

%primo al sicuro, due in viaggio.
costo(in(arrivato(X),viaggio(Y,C1)),in(arrivato(X),arrivato(Y)),C):-
	C is C1,!.

%secondo al sicuro,primo in incrocio
costo(in(arrivato(X),arrivato(Y)),in(arrivato(Z),arrivato(Y)),C):-
	connesso(X,Z,C2),
	C is C2,!.

%Gruppo due arrivato al sicuro, uno in viaggio.
costo(in(viaggio(X,C1),arrivato(Y)),in(arrivato(X),arrivato(Y)),C):-
	C is C1,!.


%In viaggio il primo gruppo, arriva primo gruppo
costo(in(viaggio(X,C1),arrivato(Y)),in(arrivato(X),viaggio(Q,C2)),C):-
	connesso(Y,Q,C3),
	C is C1+(C3-C2),!.

%In viaggio il primo gruppo, arriva il secondo
costo(in(viaggio(X,C1),arrivato(Y)),in(viaggio(X,C2),arrivato(Q)),C):-
	connesso(Y,Q,C3),
	C is C3+(C1-C2),!.

%In viaggio il primo gruppo,arrivano entrambi
costo(in(viaggio(X,C1),arrivato(Y)),in(arrivato(X),arrivato(Q)),C):-
	connesso(Y,Q,C2),
	C is C1 + C2,!.

%In viaggio il secondo, arriva il primo
costo(in(arrivato(X),viaggio(Y,C1)),in(arrivato(Z),viaggio(Y,C2)),C):-
	connesso(X,Z,C3),
	C is C3+(C1-C2),!.

%In viaggio il secondo, arriva il secondo
costo(in(arrivato(X),viaggio(Y,C1)),in(viaggio(Z,C2),arrivato(Y)),C):-
	connesso(X,Z,C3),
	C is C1 + (C3-C2) ,!.

%In viaggio il secondo, arrivano entrambi
costo(in(arrivato(X),viaggio(Y,C1)),in(arrivato(Z),arrivato(Y)),C):-
	connesso(X,Z,C2),
	C is C1+C2,!.

%Partono entrambi da incrocio, il primo arriva a dest.
costo(in(arrivato(X),arrivato(Y)),in(arrivato(Z),viaggio(Q,C1)),C):-
	connesso(X,Z,C2),
	connesso(Y,Q,C3),
	C is C2 + (C3-C1),!.

%Partono entrambi da incrocio, il secondo arriva a dest.
costo(in(arrivato(X),arrivato(Y)),in(viaggio(Z,C1),arrivato(Q)),C):-
	connesso(X,Z,C2),
	connesso(Y,Q,C3),
	C is C3 + (C2-C1),!.

%Partono entrambi da incrocio, arrivano entrambi in incrocio
costo(in(arrivato(X),arrivato(Y)),in(arrivato(Z),arrivato(Q)),C):-
	connesso(X,Z,C1),
	connesso(Y,Q,C2),
	C is C1+C2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	EURISTICA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%h(in(arrivato(X),arrivato(Y)),H):-distEuc(X,D1),distEuc(Y,D2),H is D1 + D2.
%h(in(arrivato(X),viaggio(Y,L)),H):-distEuc(X,D1),distEuc(Y,D2),H is D1 +(D2-L).
%h(in(viaggio(X,L),arrivato(Y)),H):-distEuc(Y,D1),distEuc(X,D2),H is D1 +(D2-L).


h(in(arrivato(X),arrivato(Y)),H):-distMan(X,D1),distMan(Y,D2),H is D1 + D2.
h(in(arrivato(X),viaggio(Y,L)),H):-distMan(X,D1),distMan(Y,D2),H is D1 +(D2-L).
h(in(viaggio(X,L),arrivato(Y)),H):-distMan(Y,D1),distMan(X,D2),H is D1 +(D2-L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%

eq(in(X,Y),in(X,Y)).

%Minimizzo il comando, non inserire punti dopo il nome dell'incrocio.

risolvi(Z):-write('Stato primo gruppo: '),read(X),
	    write('Stato secondo gruppo: '),read(Y),
	    (sol(X,Y,Z)).

sol(X,Y,Z):-solve(in(arrivato(X),arrivato(Y)),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sicuro(inc18).

pericolo(inc3).

connesso(inc1,inc2,40).
connesso(inc1,inc5,40).
connesso(inc2,inc3,30).
connesso(inc2,inc6,40).
connesso(inc3,inc4,30).
connesso(inc3,inc10,60).
connesso(inc4,inc7,40).
connesso(inc5,inc6,40).
connesso(inc5,inc8,30).
connesso(inc6,inc9,30).
connesso(inc7,inc11,30).
connesso(inc8,inc9,40).
connesso(inc8,inc13,50).
connesso(inc9,inc10,30).
connesso(inc9,inc14,50).
connesso(inc10,inc11,30).
connesso(inc10,inc15,50).
connesso(inc11,inc12,30).
connesso(inc11,inc16,50).
connesso(inc12,inc17,50).
connesso(inc13,inc14,40).
connesso(inc13,inc18,60).
connesso(inc14,inc15,30).
connesso(inc14,inc19,60).
connesso(inc15,inc16,30).
connesso(inc16,inc17,30).

%connesso(inc18,inc19,4).

%Da fare i reciproci

connesso(inc2,inc1,40).
connesso(inc5,inc1,40).
connesso(inc3,inc2,30).
connesso(inc6,inc2,40).
connesso(inc4,inc3,30).
connesso(inc10,inc3,60).
connesso(inc7,inc4,40).
connesso(inc6,inc5,40).
connesso(inc8,inc5,30).
connesso(inc9,inc6,30).
connesso(inc11,inc7,30).
connesso(inc9,inc8,40).
connesso(inc13,inc8,50).
connesso(inc10,inc9,30).
connesso(inc14,inc9,50).
connesso(inc11,inc10,30).
connesso(inc15,inc10,50).
connesso(inc12,inc11,30).
connesso(inc16,inc11,50).
connesso(inc17,inc12,50).
connesso(inc14,inc13,40).
connesso(inc18,inc13,60).
connesso(inc15,inc14,30).
connesso(inc19,inc14,60).
connesso(inc16,inc15,30).
connesso(inc17,inc16,30).
connesso(inc19,inc18,40).




%%Distanze euclidee%%
%
distEuc(inc1,180).
distEuc(inc2,184).
distEuc(inc3,193).
distEuc(inc4,205).
distEuc(inc5,140).
distEuc(inc6,145).
distEuc(inc7,172).
distEuc(inc8,110).
distEuc(inc9,117).
distEuc(inc10,130).
distEuc(inc11,148).
distEuc(inc12,170).
distEuc(inc13,60).
distEuc(inc14,72).
distEuc(inc15,92).
distEuc(inc16,116).
distEuc(inc17,143).
distEuc(inc18,0). %OVVIO
distEuc(inc19,40).
%
%%%%%%%%%%%%%%%%%%%%


%%Distanze Manhattan%%
%
distMan(inc1,180).
distMan(inc2,220).
distMan(inc3,250).
distMan(inc4,280).
distMan(inc5,140).
distMan(inc6,180).
distMan(inc7,240).
distMan(inc8,110).
distMan(inc9,150).
distMan(inc10,180).
distMan(inc11,210).
distMan(inc12,240).
distMan(inc13,60).
distMan(inc14,100).
distMan(inc15,130).
distMan(inc16,160).
distMan(inc17,190).
distMan(inc18,0). %OVVIO
distMan(inc19,40).
%
%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%inizio prova%%%%%%%%%%%%%%%%%
%
:-use_module(library(time)).

pred percorso(incrocio,incrocio,incrocio).

percorso(X,Y,Z):-
	get_time(T1),
	sol(X,Y,Z),
	get_time(T2),
	writeln('-----'),
	%maplist(write,['Costo del Tragitto: ',C,'\n']),
	%writeln('-----'),
	%maplist(writeln,RPath),
	%writeln('-----'),
	T is T2-T1,
	write('Tempo di Computazione: '),
	write(T),writeln('sec.'),
	statistics(globalused,GlobalStack),
	statistics(localused,LocalStack),
	write('Stack Globale: '),write(GlobalStack),writeln('Bytes'),
	write('Stack Locale: '),write(LocalStack),writeln('Bytes').
	%retractall(inc(st(_,_,_,_,_))).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




















































