%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST INTERFACCIA %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_problem(F) :-consult(F).
show :-	writeln('Esecuzione in Corso.'),
	listaComandi.
listaComandi:-writeln('Comandi: r: solve(X,Y); h:help').

comandi :- readln(F),
	(
	F = [], !;
	F = [r|_],!,
	    write('Inserisci stato di partenza gruppo1: '),
	    readln(X),
	    write('Inserisci stato di partenza gruppo2: '),
	    readln(Y),solve(in(X,Y),Z),
	    write('. Computazione Terminata.');
	F = [h|_],!,listacomandi,command;
	F = [a|_],!,abort;
	true
	).



