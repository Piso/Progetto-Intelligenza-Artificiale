%%%%%%   Adattato al corso di IA 07/08,   Mario Ornaghi,  ott 2007

:- multifile(writerror/1).


% To be used for predicates to be ignored: loading directives etc.
% You can add new definitions for external, when needed
% Added by MO

:- multifile(no_check(_)). 
:- dynamic(ext/1).
:- retractall(ext(_)).

mk_external(L) :-
    retractall(ext(_)),
    forall(member(D,L), assert(ext(D))).

external(consult(_)).  %% external, it is not type-checked
external(op(_,_,_)).
external(no_check(_)).
external(X) :- ext(X).
external(use_module(_)).
external(use_module(_,_)).

%% Verbose flag, added by MO
%
:- dynamic(verbose_flag/0).

mk_verbose :-
	assert(verbose_flag).
mk_silent :-
	retractall(verbose_flag).

%%%%%%%%%%%%%%%%%%%%%%%%%% 

% ----------------------------------------------------------------------
% 	Typed Prolog: Type Checking & Reconstruction (TOP LEVEL)
% 	Created by T.K. Lakshman, University of Illinois, Feb 20, 1991
% ----------------------------------------------------------------------
% * Loads a file:  
% 	a. Loads the declarations for (T)ypes,(F)unctions and (P)redicates
% and	b. Loads the Program clauses.
% * Checks if the program is well-formed (well_formed.p) w.r.t. T,F and P.
% * If P is not completely specified, reconstructs the P (if possible) 
%   that makes the program well-formed.
% * Print the TYPES  T,F and P and the program
% * Writes the TYPES  T,F and P and the program on to : 
% * 	a. "file.t" if a single file name was specified as input
% *	b. "output.t" if a list of files were specified as input
% Bug fix: 4/20/93 (thanks to Florence Pagani) in defn of type-preserving
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Operators used in type declarations.
%:- op(1199, fx, [(type), (pred)]).
%:- op(998, xfy, (-->)).
%:- op(997, xfy, (;)).
% ----------------------------------------------------------------------
% TOP LEVEL LOAD (modified at Mattias Waldau's suggestion 3/2/91)
%
%	
% a.	The rload/0 predicate asks the user to specify a file name and 
% 	reads the Typed Prolog program from this file.

rload :- 
	write(' Please specify input file name: '),
	read(File),	
	write(' ...Reading declarations and program clauses '), nl,
	get_from(File,Td,Fd,Pd,Program),% Read Prog + Decls
	output_file_name(File,FName),   % FName is X  or 'output'
        process_input(FName,Td,Fd,Pd,Program).	% Process the Program


% b.	The rload/1 predicate takes a file or a list of files as an
%	argument and reads/processes a Typed Prolog program from these files.
% 
% 	e.g. rload([libdecls,File1,File2]) can be used to  load the
%	library of type declarations (libdecls) and then load the
% 	programs from File1 and File2.

rload(X) :- 
	write(' ...Reading declarations and program clauses '), nl,
	get_from(X,Td,Fd,Pd,Program),% Read Prog + Decls
	output_file_name(X,FName),   % FName is X  or 'output'
        process_input(FName,Td,Fd,Pd,Program).	% Process the Program

get_from(Var,_Td,_Fd,_Pd,_Program) :- 
        var(Var),
        !,
        write('#$%^&>> rload: argument contains a variable'), 
	write(' .Cannot continue'), nl, abort.

get_from([],_Td,_Fd,_Pd,_Program) :- !.	


get_from([File|FileList],Td,Fd,Pd,Program) :- !,
        get_from(File,Td,Fd,Pd,Program),   
        get_from(FileList,Td,Fd,Pd,Program).

get_from(File,Td,Fd,Pd,Program) :-
        atom(File), !,
        read_input(File,Td,Fd,Pd,Program). 	% Read from the source file

get_from(File,_Td,_Fd,_Pd,_Program) :-
        write('#$%^&>> rload: file name not list or atom: '),
        write(File), 
	write(' .Cannot continue'), nl, abort.
% ----------------------------------------------------------------------
output_file_name([],output) :- !.
output_file_name(X,NX) :- atom(X), !,
	concat(X, '_out.pl',NX).
output_file_name(_X,'out.pl').
% ----------------------------------------------------------------------
process_input(File,Td,Fd,Pd,Program) :- 

% * 0. Program & Fns have been constructed (NO RECONSTRUCTION HERE)
	ground_list(Program), ground_list(Fd),

% * 1. a. Rename Program for output purposes only.
	rename_prog(Program,ProgRenamed), 	

%   1. b. Rename Predicate Definitions for "DEFINITIONAL GENERICITY" test.
        rename_pred(Pd,PdRenamed),

% * 2. The actual type-checking/reconstruction procedure
        write(' ... doing Type Checking / Reconstruction ') , nl,
	well_formed_program(Program,program,_,Td,Fd,Pd),

% * 3. The test for definitional genericity.
%       Reconstructed type Pd should be AS GENERAL as  input type PdRenamed
	ground_list(Pd), ground_list(PdRenamed), 
        def_genericity(Pd,PdRenamed),

% * 4. Print the Program and the Types for T, F and P (on the Screen).
	(   
	 verbose_flag -> 
	    (write('Type Constructors    : '), nl, pp_type(Td), nl,
	     write('Types for Functions  : '), nl, pp_fun(Fd),nl,
	     write('Types for Predicates : '), nl, pp_pred(Pd),nl,
	     write('Program   : '), nl, pp_prog(ProgRenamed), nl,nl);
	     true
	),

% * 5. Write the Program and Types for T, F and P to the file
	write_file(File,Td,Fd,Pd,ProgRenamed).


% ----------------------------------------------------------------------
% Auxiliary functions: used by the above program and well_formed.p
% ----------------------------------------------------------------------
% * Loads a file  
read_input(File,Td,Fd,Pd,Program) :- nl,
	concat(File,'.pl',NFile),
	see(NFile),
	read(Cl),				% first clause
	transform(Cl, Td, Fd, Pd, Program),
	read_program(Cl, Td, Fd, Pd, Program),
        seen.

% ----------------------------------------------------------------------
% Split up each clause read into 
%	1. Type declaration for T, F or P ==> store it in Td, Fd, or Pd
% 	2. Program clause ==> store it in Program.
% ----------------------------------------------------------------------

% 0. Directives to be ignored 
% to make this system compatible with  Mycroft OKeefe type-checker syntax.
transform((:- Directive), _Td,_Fd,_Pd,_Program) :- 
	external(Directive), !.

% 1. Declarations:  library declarations 
% to make this system compatible with  Mycroft OKeefe type-checker syntax.
transform((:- Declarations), Td,Fd,Pd,Program) :- 
	transform(Declarations,Td,Fd,Pd,Program).

% 2. Type Constructor declarations and Function type declarations
transform(type('-->'(Type, Constructors)) ,Td, Fd,_Pd, _Program) :- 
% a. Process the Type cunstructor
	functor(Type,Functor,Arity),
	Tcons =.. [Functor,Arity],
	insert_unique(Tcons,Td), % Tcons: type-constructor declaration in Td.
% b. Process the function symbols
	func_transform(Type, Constructors,Fd).

% 2. Just Type constructors : no function symbols
transform(type(Type) ,Td, _Fd,_Pd, _Program) :- 
	functor(Type,Functor,Arity),
	Tcons =.. [Functor,Arity],
	insert_unique(Tcons,Td). % Tcons: type-constructor declaration in Td.

% 3. Predicate type declaration
% Transform (pred p(t*) to pair(p,pred([t*]))

transform(pred((P1 ; P2)), Td, Fd, Pd, Program) :- 
	transform(pred(P1), Td, Fd, Pd, Program),
	transform(pred(P2), Td, Fd, Pd, Program).


transform(pred({Pred}), _Td, _Fd, Pd, _Program) :- 
	Pred =.. [P|LType], insert_unique_p(pair(P,pred(LType)),Pd).
transform(pred(X), _Td, _Fd, Pd, _Program) :- 
	X =.. [Pred|LType], insert_unique_p(pair(Pred,pred(LType)),Pd).


% 4. End of file: 
transform(end_of_file,_Td, _Fd, _Pd, _Program).


% 5. Else it is a program clause
transform(Cl, _Td, _Fd, _Pd, Program) :- insert(Cl, Program).

% ----------------------------------------------------------------------

% Process the function symbols on the Rhs of  type Lhs --> Rhs
% Insert their types in Fd.

func_transform(Type, (Cons1;Cons2),Fd) :- 
	f_transform(Type, Cons1, Fd),
	func_transform(Type,Cons2,Fd).

func_transform(Type, Cons1,Fd) :- f_transform(Type,Cons1,Fd).

% Process a function symbol (CHECK IF a fn symbol is TYPE PRESERVING)
% ----------------------------------------------------------------------
f_transform(Type,Term,Fd) :- 
	Term =.. [Functor | ArgList],
	type_preserving(Functor,ArgList,Type), !, 
	insert_unique_p(pair(Functor,fun(ArgList,Type)), Fd).

f_transform(_Type,Term,_Fd) :- 
	Term =.. [Functor | _ArgList],
	throw(crError(not_type_preserving(Functor))).

%%%%%%%%%%%%%%%%------>  throw and writerror inserted by M.O., oct 2007

writerror(crError(not_type_preserving(Functor))) :-
	write('>> Function symbol '), write(Functor), 
	write(' is NOT type preserving; Cannot continue '), nl, abort.


% TYPE PRESERVING fns: Let f:: lhs -> rhs
% f is type preserving if vars(lhs) <= vars(rhs)
% BUG POINTED OUT by Florence Pagani (pagani@erato.cert.fr)
% The code was checking vars (rhs) <= vars(lhs) this is INCORRECT
% type-preserving <==> vars(lhs) <= vars(rhs)

type_preserving(_F,[],_Rhs) :- !.
type_preserving(_F,LhsList,Rhs) :- 
        number_vars(Rhs, 0, M),
        numberlist(LhsList, M, N),
        N > M,
        !,
       write('! lhs of type def contains variables not appearing in rhs.'), nl,
        fail.
type_preserving(_F,_LhsList,_Rhs).

% ----------------------------------------------------------------------
% DEFINITIONAL GENERICITY TEST
% Ensures that the reconstructed type for predicates are as general as the 
% defined types (and NOT a specific instance)

% ASSUMES that Pd is an INITIAL segment of PdRecon: this is justified
% since well_formed/6 takes Pd as input and returns PdRecon where
% the newly reconstructed predicate types are appended to the end...


def_genericity(_PdRecon,[]):- !.

def_genericity([pair(P,pred(PRType))|L1],[pair(P,pred(PType))|L]):- 
	is_renaming(PRType,PType), def_genericity(L1,L).

def_genericity([pair(P,PRType)|_L1],[pair(P,PType)|_L]):- 
	throw(crError(def_generity_violated(P,PRType,PType))).
	      
def_genericity([pair(P,PRType)|_L1],[pair(Q,PType)|_L]):- !,nl,
	throw(crError(def_generity_notdefined(P,PRType,Q,PType))).

%%%------------------>   throw and writerror inserted by M.O., oct 2007

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

% ----------------------------------------------------------------------
% is_renaming(pred([t1,...,tn]),pred([u1,..,un]))
% returns true if pred(t1,...,tn) is a VARIANT of pred(u1,..,un)

is_renaming([],[]).
is_renaming([H|T],[H1|T1]) :- variant(H,H1), !, is_renaming(T,T1).

% variant(T1,T2) returns true if T1 is a variant of T2
variant(X,Y) :- var(X),!, var(Y).
variant(T1,T2) :- T1 =..[F|L] ,T2 =.. [F|L1], !, variant_list(L,L1).
% Alternative suggested by luis hermosilla (ECRC) to avoid the
% partial list error (ISO/BSI)
% variant(T1,T2) :- nonvar(T2),	% T1 and T2 are not variables
%	T1 =.. [F|L],T2 =.. [F|L1],variant_list(L,L1).

variant_list([],[]).
variant_list([H|T],[H1|T1]) :- variant(H,H1), ! , variant_list(T,T1).

% ----------------------------------------------------------------------
% ----------------------------------------------------------------------

% Read a Typed Prolog program, one clause at a time.
read_program(Cl,_Td, _Fd, _Pd, _Program) :- Cl = end_of_file, !.
read_program(_Cl,Td, Fd, Pd, Program) :- 
	read(NewCl), transform(NewCl, Td, Fd, Pd, Program),
	read_program(NewCl, Td, Fd, Pd, Program).


% ----------------------------------------------------------------------

% Write the Program and Types for T, F and P to the file  File.t.
write_file(File, Td, Fd, Pd, ProgRenamed) :- 
	%name(File,L),append(L,[46,116],L1),name(OutFile,L1),
	nl,
	write('*** SIGNATURE OUT FILE ----> '), write(File),nl,nl,

	tell(File),

	write('% Type Constructors    : '), nl, nl,
	st_type(Td), nl, 

	write('% Types for Functions  : '), nl, nl,
	st_fun(Fd),nl, 

	write('% Types for Predicates: '), nl, nl,
	st_pred(Pd) ,nl, 

	write('% Program   : '), nl, nl,
	st_prog(ProgRenamed), nl,

	told.	

% ----------------------------------------------------------------------
% pretty print
% ----------------------------------------------------------------------
pp_type([]).
pp_type([H|T]) :- 
	write('type       '),
	write(H), write('.'), nl, 
	pp_type(T).

pp_fun([]).
pp_fun([pair(X,fun(L,R))|T]) :- 
	write('type       '),
	write(X), write(':  '),
	write(L),
	write('    -->     '),
	write(R), write('.'), nl, 
	pp_fun(T).


pp_pred([]).
pp_pred([pair(X,Var)|T]) :- var(Var), write('% predicate '), write(X), 
	write(' not defined in the program '), nl, pp_pred(T).
pp_pred([pair(X,pred(L))|T]) :- 
	write('pred       '), ground_list(L),
	Y =.. [X|L], write(Y),
	write('.'), nl, 
	pp_pred(T).

pp_prog([]).
pp_prog([C|Cl]) :- 
	write('             '),
	write(C), write('.'), nl, 
	pp_prog(Cl).
% ----------------------------------------------------------------------

%%	Added by MO:  the output is a prolog program to be used with
%       the generator program

st_type([]).
st_type([H|T]) :- 
	write_atm(type(H)),
	st_type(T).

st_fun([]).
st_fun([pair(X,fun(L,R))|T]) :- 
	write_atm(fun(X,L,R)),
	st_fun(T).


st_pred([]).
st_pred([pair(X,Var)|T]) :- 
	to_be_ignored(Var),
	write('%  ************ ignored: '),
	writeln(X),
	st_pred(T).

st_pred([pair(X,pred(L))|T]) :- 
	ground_list(L),
	write_atm(pred(X,L)), 
	st_pred(T).

st_prog([]).
st_prog([C|Cl]) :- 
	write_clause(C), 
	st_prog(Cl).

write_atm(X) :-
	write(X),
	write('.'),
	nl.

write_clause(C) :- 
	not(C = no_check(_)) *-> write_atm(C); true. 

to_be_ignored(Var) :- var(Var).












