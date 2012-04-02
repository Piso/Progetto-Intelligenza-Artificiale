:-consult(robot).
:-consult(best_first).
%:-consult(astar).
:-use_module(library(time)).

piano(Ordini):-
	list_to_ord_set(Ordini,OrdL),
	%creo la lista di fluenti partendo dagli ordini
	%usando maplist e deve
	maplist(deveP,OrdL,OrdF),
	get_time(T1),
	solve(st(b,b,OrdF,0,0),nc(NodoFinale,Path,Costo)),
	get_time(T2),
	reverse([NodoFinale|Path],RPath),
	writeln('----------'),
	maplist(write,['Piano con costo: ',Costo,'\n']),
	writeln('-----'),
	maplist(writeln,RPath),
	writeln('-----'),
	%stampo un pò di statistiche
	T is T2-T1,
	write('Tempo di calcolo: '),
	write(T),writeln('sec.'),
	statistics(globalused,GlobalStack),
	statistics(localused,LocalStack),
	write('Stack globale: '),write(GlobalStack),writeln('Bytes'),
	write('Stack locale: '),write(LocalStack),writeln('Bytes'),
	retractall(inc(st(_,_,_,_,_))).


deveP(Ordine,dp(Ordine)).

















































