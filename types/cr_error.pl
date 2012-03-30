:- module(cr_error, [writerror/1]).

% for cr_io:

writerror(crError(not_type_preserving(Functor))) :-
	write('>> Function symbol '), write(Functor), 
	write(' is NOT type preserving; Cannot continue '), nl, abort.

writerror(def_generity_violated(P,PRType,PType)):-      
        nl, write('>> Definitional Genericity violated'), nl, nl,
	write(' Reconstructed type for '), write(P), write(':'), 
	write(PRType), write(' is more specific than '), nl,
        write(' the declared type for '), write(P), write(':'), 
        write(PType),nl, nl,
        write(' .Cannot continue '), nl, abort.

writerror(def_generity_notdefined(P,_PRType,_Q,_PType)) :-
        write('>> predicate type for '), P,
	write('is reconstructed but not defined'),nl,
        write(' .Cannot continue '), nl, abort.

% for auxo
% 

writerror(not_decl_fun(X)) :-
        write('>> function symbol: '), write(X), 
	write(' not declared. Cannot continue '), nl, abort.

writerror(type_conflict(T,U)):-
	nl, write('>> Type Conflict in types    '), 
	write(T), write('   and   '), write(U), nl, abort.

writerror(type_conflict(Type,TypeL)) :-
         nl, write('>> Type Conflict in types    '), 
  	 write(Type), write('   and   '), write(TypeL), nl, abort.
