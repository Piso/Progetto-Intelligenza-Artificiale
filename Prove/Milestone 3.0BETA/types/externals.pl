:- module(externals, [external/1,
		      executable/1,
		      exec_external/1
		      ]).

:- dynamic(ext/1). 

external(consult(_)).  %% external, it is not type-checked
external(use_module(_)).
external(ensure_loaded(_)).
external(module(_)).
external(use_module(_,_)).
external(module(_,_)).
external(op(_,_,_)).
external(assert(_)).
external(retract(_)).
external(retractall(_)).
external(no_check(_)).
external(check(_)).
external(dynamic(_)).
external(write(_)).
external(writeln(_)).
external(read(_)).
external(readln(_)).
external(_ -> _).
external(_ *-> _).
external(X) :- ext(X).

executable(check(_)).
executable(no_check(_)).

exec_external(no_check(E)) :- !,
	writeln('declared external':E),
        (
	is_list(E) ->
	    forall(member(X,E), assert1(ext(X)))
	    ;
	assert1(ext(E))
	).

exec_external(check(E)) :-
	retractall(ext(E)).

assert1(E) :-
	E,!;
	assert(E).




