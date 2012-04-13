:-consult(evacuazioneincorso).
:-consult(best_first).
%:-consult(astar).
:-use_module(library(time)).

percorso(N):-
	get_time(T1),
	solve(inc1,inc4,X),
	get_time(T2),
	writeln('----------'),
	maplist(write,['Costo del Tragitto: ',C,'\n']),
	writeln('-----'),
	%maplist(writeln,RPath),
	writeln('-----'),
	T is T2-T1,
	write('Tempo di Computazione: '),
	write(T),writeln('sec.'),
	statistics(globalused,GlobalStack),
	statistics(localused,LocalStack),
	write('Stack Globale: '),write(GlobalStack),writeln('Bytes'),
	write('Stack Locale: '),write(LocalStack),writeln('Bytes'),
	%retractall(inc(st(_,_,_,_,_))).
