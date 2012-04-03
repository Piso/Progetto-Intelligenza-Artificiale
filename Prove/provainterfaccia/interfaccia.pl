%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST INTERFACCIA %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
:-dynamic(showflag/0).

% type float --> 0; 1; float + float; float-float; float/float; float *
% float.

%type stato.

%type nodo--> nc(stato,list(stato),float).
%

load_problem(F) :-consult(F).
%:-consult(restart). errato.
%load_problem(restart).

:-writeln('Inizio Programma. Esecuzione interfaccia.').
:-writeln('Comandi: r:solve(X,Y); h:help; a:esci').

%%pred solve(stato,nodo).

%show:-writeln('Esecuzione in Corso.'),listaComandi.

listaComandi:-assert(showflag),writeln('Comandi: r:solve(X,Y); h:help; a:esci').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%creaIncrocio(X):-(
	%      X = [HEAD|.], X = HEAD,display(HEAD);
	 %     true
	  %    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
creaSoluzione(X,Y,Z):-display(solve((in(X,Y)),Z)).

comandi :- readln(F),
	(
	F = [],!,comandi;
	F = [r|_],!,
	    write('Inserisci stato di partenza gruppo1: '),
	    readln(X),
	    write('Inserisci stato di partenza gruppo2: '),
	    readln(Y),creaSoluzione,
	    writeln('. Computazione Terminata.'),comandi;
	F = [h|_],!,listaComandi,comandi;
	F = [a|_],!,abort;
	true
	).

%:-comandi.













