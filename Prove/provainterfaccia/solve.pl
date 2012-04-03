%Minimizzo il comando, non inserire punti dopo il nome dell'incrocio.
%
%
%


risolvi(Z):-write('Stato primo gruppo: '),readln(X),
	    assert(X,Primo),
	    write('Stato secondo gruppo: '),readln(Y),
	    assert(Y,Secondo),
	   % write('Soluzione: '),
            %readln(Z),
	    trovaSol(Z,Primo,Secondo).
trovaSol(Z,Primo,Secondo):-solve(in(Primo,Secondo),Z).
