% ----------------------------------------------------------------------
% 	Typed Prolog: Type Checking & Reconstruction (AUXILIARY FUNCTIONS)
% 	Created by T.K. Lakshman, University of Illinois, Feb 20, 1991.
% ----------------------------------------------------------------------
% list lookup (for types of fns/preds)
% 

assoc(X,Type,[pair(X,Type)|_]).
assoc(X,Type,[pair(Z,_)|L]) :- X \== Z, assoc(X,Type,L).
assoc(X,_Type,[]) :- throw(crError(not_decl_fun(X))).

%%%%  Adapted by M.O. oct 2007: throw and writerror added
% writerror(not_decl_fun(X)) 


% -------------------------------
% rename the variables in a term.
% -------------------------------
% rename_var(T,U):  U is T with variables renamed.
rename_var(T,U) :- 
	freeze(T,T1,0,_N,_L),	% rename vars to numbers like $(num)
	reconstruct(T1,U,_TBL).

rename_var(T,U) :- throw(crError(type_conflict(T,U))).
%%%%  Adapted by M.O. oct 2007: throw and writerror added
%writerror(type_conflict(T,U))
%	



% reconstruct(var,new-var,table) : var --> new-var if (var,new-var)  in table
% else for TERM recursively reconstruct(arg#,total-#args,term, new-term,table)
reconstruct($(N),X,TBL) :- !,in_table(($(N),X),TBL).
reconstruct(T,U,TBL) :-
	functor(T,F,N),
	functor(U,F,N),
	reconstruct(0,N,T,U,TBL).

% reconstruct(arg#,total-#args,term, new-term,table)
reconstruct(N,N,_T,_U,_TBL).	% termination condition
reconstruct(N,M,T,U,TBL) :-
	N1 is N + 1,
	arg(N1,T,X),
	arg(N1,U,Y),
	reconstruct(X,Y,TBL),
	reconstruct(N1,M,T,U,TBL).

% binary search tree
in_table((X,Y),b((X,Y),_L,_R)) :- !.	% found
in_table(($(X),Y),b(($(U),_V),L,_R)) :-	% go left
	X < U,
	!,
	in_table(($(X),Y),L).
in_table(E,b(_Root,_L,R)) :-		% go right
	in_table(E,R).

% freeze variables in T to get U; variable --> $(n) where N <= n < N1
freeze(T,U,N,N1,L) :- var(T), !,  lookup(T,U,N,N1,L).
freeze(T,U,N,N1,L) :- 
	T =.. [F|T1],!,
	freezelist(T1,U1,N,N1,L),
	U =.. [F|U1].

freezelist([],[],N,N,_L).
freezelist([X|T],[Y|U],N,N1,L) :-
	var(X), !, lookup(X,Y,N,N2,L),
	freezelist(T,U,N2,N1,L).
freezelist([X|T],[Y|U],N,N1,L) :-
	X =.. [F|Thead],
	freezelist(Thead,Uhead,N,N2,L),
	Y =..[F|Uhead],
	freezelist(T,U,N2,N1,L).

% list lookup for variables: similar to var_assoc.
% all this is to avoid the occurs check problems in variable lookup.
% lookup(var,new_var,N,N1,List)
lookup(X,Y,N,N1,L) :- found(X,Y,N,L), N1 is N , !.
lookup(X,Y,N,N1,L) :- var(Y), insert(X,Y,N,L), N1 is N + 1.

found(_X,_Y,_N,L) :- var(L), !, fail.
found(X,Y,_N,[pair(Z,NZ)|_]) :- X == Z, Y = $(NZ).
found(X,Y,N,[pair(Z,_)|L]) :- X \== Z, found(X,Y,N,L).

insert(X,Y,N,L) :- var(L), L = [pair(X,N)|_], Y = $(N).
insert(X,Y,N,[pair(_,_)|L]) :- insert(X,Y,N,L).

% ------------------------ END OF RENAME ----------------------------

% list lookup for variables 
% all this is to avoid the occurs check problems in variable lookup.

var_assoc(X,Type,L) :- found(X,L,TypeL), !, type_match(Type,TypeL).
var_assoc(X,Type,L) :- insert(X,Type,L).

% BEWARE: (thanks andre de'wall) Would cause occurs-check for rec. type eqns.

type_match(Type,TypeL) :- Type = TypeL. 
type_match(Type,TypeL) :- throw(crError(type_conflict(Type,TypeL))).

%%%%---->  Adapted oct 2007.  throw and writerr added
% writerror(type_conflict(Type,TypeL))

			
		

found(_X,Z,_) :- var(Z), !, fail.
found(X,[pair(Z,TypeZ)|_],TypeZ) :- X == Z.
found(X,[pair(Z,_)|L],TypeZ) :- X \== Z, found(X,L,TypeZ).

insert(X,Type,Z) :- var(Z), Z = [pair(X,Type)|_].
insert(X,Type,[pair(_,_)|L]) :- insert(X,Type,L).

occurs_in(Term,L) :- Term =.. [H|_], member(H,L).

% ----------------------------------------------------------------------

rename_prog([],[]).
rename_prog([C|Cl],[Cnew|Clnew]) :- rename_var(C,Cnew), rename_prog(Cl,Clnew).

rename_pred(X,_Y) :- var(X), !.
rename_pred([P|Pd],[Pnew|Pdnew]) :- rename_var(P,Pnew), rename_pred(Pd,Pdnew).
rename_pred(_X,_Y) :- !.

ground_list([]) :- !.
ground_list([_|L]) :- ground_list(L).
% ----------------------------------------------------------------------
% number_vars(Term,Lo,Hi) : pred number_vars(T, integer, integer).
% mode number_vars(i,i,o).
% Used to check whether function definitions are type preserving.
% numbers the variables in Term from Lo to Hi.

number_vars(Term,Lo,Hi) :- var(Term), !, Term = Lo, Hi is Lo + 1.
number_vars(Term,Lo,Hi) :- Term =.. [_|L], !, numberlist(L,Lo,Hi).

numberlist([],Lo,Lo).
numberlist([X|L], Lo, Hi) :- var(X), !, X = Lo, Next is Lo + 1, 
			     numberlist(L,Next,Hi).
numberlist([X|L], Lo, Hi) :- X =.. [_|Lhead], numberlist(Lhead,Lo,Hi1),
			     numberlist(L,Hi1,Hi).
% ----------------------------------------------------------------------
% The following code detects overloading and signals a warning
% ----------------------------------------------------------------------
% insert_unique(elem,List)
% reports error and IGNORES elem if elem is already in the list
% else does normal insert.

insert_unique(X,L) :- var(L), L = [X|_].
insert_unique(X,[X|_L]) :- write(' Warning: Multiply defined type constructor'), nl,
	write('       Ignoring the latter type  definition for: '), 
	write(X) , nl.
insert_unique(X,[_Y|L]) :- insert_unique(X,L).

% insert_unique_p(elem,List): ditto except elem is of the form pair(Symbol,_)

insert_unique_p(X,L) :- var(L), L = [X|_].
insert_unique_p(pair(Symbol,_),[pair(Symbol,_)|_L]) :- 
	write(' Warning: Multiply defined symbol'),nl,
	write('       Ignoring the latter type definition for the symbol: '), 
	write(Symbol) , nl.
insert_unique_p(X,[_Y|L]) :- insert_unique_p(X,L).

% ----------------------------------------------------------------------



