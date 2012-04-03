%Minimizzo il comando, non inserire punti dopo il nome dell'incrocio.
risolvi(Z):-write('Stato primo gruppo: '),readln(X),
	    write('Stato secondo gruppo: '),readln(Y),
	    trovaSol(Z).
trovaSol(Z):-solve(in(X,Y),Z).
