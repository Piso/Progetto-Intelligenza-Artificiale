%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST INTERFACCIA %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
:-dynamic(showflag/0).


:-writeln('Inizio Programma. Caricamento esecuzione interfaccia.').
:-writeln('Comandi:  load_problem(+F:nome_file), solve, show, noshow\n\n').

solve(_,Y):-trovato(Y).

load_problem(F) :-consult(F).
show :-	writeln('Esecuzione in Corso.'),
	listaComandi.
listaComandi:-writeln('Comandi: r: solve(X,Y); h:help').


comandi :- readln(F),
	(
	F = [], !;
	F = [r|_],
	    write('Inserisci stato di partenza gruppo1: '),
	    readln(X),
	    write('Inserisci stato di partenza gruppo2: '),
	    readln(Y),solve(in(X,Y),_),
	    write('. Computazione Terminata.');
	F = [h|_],!,listaComandi,comandi;
	F = [a|_],!,abort;
	true
	).



