% ----------------------------------------------------------------------
% 	Typed Prolog: Type Checking & Reconstruction (TYPE CHECK/RECONSTRUCT)
% 	Created by T.K. Lakshman, University of Illinois, Feb 20, 1991
% ----------------------------------------------------------------------
% TYPE-CHECK: Completely Typed Program P     --> YES/NO (Is P well-formed?)
% TYPE-RECONSTRUCT: Partially Typed Program  --> Completely Typed 
%                                                well-formed Program
% ----------------------------------------------------------------------
% well_formed/6(Term,Type,Context,TypeDecl,FuncDecl,PredDecl).
% checks to see if Term is a well formed TYPED-PROLOG term 
% of type Type in a Context with declarations TypeDecl,FuncDecl and PredDecl.
% ----------------------------------------------------------------------

%adapted by M.O.:  well_formed_generic, 3.1bis  (oct 2007)

:- use_module(cr_error, [writerror/1]).
:- use_module(externals, [external/1]).

%1. Variable
well_formed(variable(X),Type,Context,_,_,_) :- 
	var_assoc(variable(X),Type,Context).


% 2. Term
well_formed(T,Type,Context,Td,Fd,Pd) :- 
	T =.. [F|Args], 
	assoc(F,Ftype,Fd),
	rename_var(Ftype,fun(ArgTypes,Type)), % computed types
		% can be different instances
		% of the declared type => POLYMORPHISM
	well_formed_list(Args,ArgTypes,Context,Td,Fd,Pd).



% well_formed/8(Term,Type,Context,TypeDecl,FuncDecl,PredDecl,Call_table,H)
% Call_table is the call graph for the program that is used to check
% whether atom is recursively defined.
% H is the head of the rule from which the atom was derived..




% 3. Atom (body) RECURSIVE
well_formed(A,atom,Context,Td,Fd,Pd,Call_table,H) :-
	A =.. [P|Args],
	in_table(P,Call_table,L), % P and H are MUTUALLY RECURSIVE
	occurs_in(H,L),		  % 
	assoc(P,pred(ArgTypes),Pd),
		% body type == declared type
		% No renaming!
	well_formed_list(Args,ArgTypes,Context,Td,Fd,Pd).


% 3. Atom (body) NON-RECURSIVE
well_formed(A,atom,Context,Td,Fd,Pd,_Call_table,_H) :-
	A =.. [P|Args],
	assoc(P,Ptype,Pd), % Type of P had better have been computed
	rename_var(Ptype,pred(ArgTypes)), % computed body types
		% can be  INSTANCES
		% of the declared type => POLYMORPHISM
	well_formed_list(Args,ArgTypes,Context,Td,Fd,Pd).

% 3'bis. New (M.Ornaghi) EXTERNAL, to be improved;
% some external symbols are statically defined in cr
% SEE BELOW

% 4. Formula 
well_formed(true,formula,_Context,_Td,_Fd,_Pd,_Call_table,_H).

% non-logical features like assert, retract, cut, not.
well_formed(!,formula,_Context,_Td,_Fd,_Pd,_Call_table,_H).

well_formed(not(A),formula,Context,Td,Fd,Pd,Call_table,H) :-
	well_formed(A,atom,Context,Td,Fd,Pd,Call_table,H).

well_formed(assert(A),formula,Context,Td,Fd,Pd,Call_table,_H) :-
	well_formed(A,clause,Context,Td,Fd,Pd,Call_table).

well_formed(retract(A),formula,Context,Td,Fd,Pd,Call_table,_H) :-
	well_formed(A,clause,Context,Td,Fd,Pd,Call_table).

well_formed((F1,F2),formula,Context,Td,Fd,Pd,Call_table,H) :- 
	well_formed(F1,formula,Context,Td,Fd,Pd,Call_table,H),
	well_formed(F2,formula,Context,Td,Fd,Pd,Call_table,H).

well_formed((F1;F2),formula,Context,Td,Fd,Pd,Call_table,H) :- 
	well_formed(F1,formula,Context,Td,Fd,Pd,Call_table,H),
	well_formed(F2,formula,Context,Td,Fd,Pd,Call_table,H).

well_formed(=(T1,T2),formula,Context,Td,Fd,Pd,_Call_table,_H) :-
	well_formed(T1,Type,Context,Td,Fd,Pd),
	well_formed(T2,Type,Context,Td,Fd,Pd).

well_formed(F,formula,Context,Td,Fd,Pd,Call_table,H) :-
	not(external(F)) *->
	      well_formed(F,atom,Context,Td,Fd,Pd,Call_table,H)
	      ;
	      true.


% 5. Clause 
% %%%% catching exceptions:  by Mario Ornaghi
well_formed(:-(Head,Body),clause,Context,Td,Fd,Pd,Call_table) :-
	%nl,writeln(:-(Head,Body)),
	catch(well_formed_generic(Head,atom,Context,Td,Fd,Pd),
	      crError(Error),
	      abortError(:-(Head,Body), Error)),
	catch(well_formed(Body,formula,Context,Td,Fd,Pd,Call_table,Head),
	      crError(Error),
	      abortError(:-(Head,Body), Error)).

well_formed(Head,clause,Context,Td,Fd,Pd,Call_table) :-
	well_formed(:-(Head,true), clause,Context,Td,Fd,Pd,Call_table).


% 3'bis. New (M.Ornaghi) EXTERNAL, to be improved;
% some external symbols are statically defined in cr

well_formed_generic(A,atom,_Context,_Td,_Fd,_Pd) :-
	external(A), 
	write('*** '),
	writeln(A).

% 3'. Atom (head)
well_formed_generic(A,atom,Context,Td,Fd,Pd) :-
        not(external(A)),
	%nl,
	%write('******* INTERNAL: '),
	%write(A),
	A =.. [P|Args],
	assoc(P,pred(ArgTypes),Pd),
	% don't use rename_var(Ptype,Ptype1) since we want
	% M.G.DECLARED and Computed Types to be same and NOT instance
	well_formed_list(Args, ArgTypes, Context,Td,Fd,Pd).

% error msg
abortError(Clause,Error) :-
	nl,write('******************  '), 
        write(Clause), write(':'), nl, writerror(Error), nl, 
	abort.


% 6. Program
well_formed_program(Program,program,_,Td,Fd,Pd) :-

% a. Compute Call_table =  call_list for each predicate that is defined.
% used to handle mutually recursive definitions
	mk_call_table(Program, Call_table, Program),

% b. Compute the Adj_list = adjacency list for each predicate that is defined
% Used to topologically sort the program
	mk_adj_list(Program,Adj_list),
% c. re-order the program clauses so that well_formed_recon can reconstruct
% a predicate type before its "use". 
	order_program(Adj_list,Program,Ord_Prog),

% d. Re-construct (if reqd) the predicate types,
% check each predicate defn for well-formedness 
	well_formed_recon(Ord_Prog,program,_,Td,Fd,Pd, Call_table).


% ----------------------------------------------------------------------
% reconstructs type of preds (if reqd)
% check each clause for well-formedness 
% well_formed_recon/7(Term,Type,Context,TypeDecl,FuncDecl,PredDecl,Call_table)

well_formed_recon([],program,_,_Td,_Fd,_Pd, _Call_table).

well_formed_recon([C|Cl],program,_,Td,Fd,Pd, Call_table) :-
	well_formed(C,clause,_,Td,Fd,Pd,Call_table),
	well_formed_recon(Cl,program,_,Td,Fd,Pd,Call_table).

% ----------------------------------------------------------------------
% List of terms
well_formed_list([],_,_,_,_,_).

well_formed_list([H|L],[TH|TL],Context,Td,Fd,Pd) :- 
	well_formed(H,TH,Context,Td,Fd,Pd),
	well_formed_list(L,TL,Context,Td,Fd,Pd).
% ----------------------------------------------------------------------











