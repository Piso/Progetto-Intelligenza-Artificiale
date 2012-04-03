% ----------------------------------------------------------------------
% 	Typed Prolog: Type Checking & Reconstruction (MAKE CALL TABLE)
% 	Created by T.K. Lakshman, University of Illinois, Feb 20, 1991.
% ----------------------------------------------------------------------
% PROGRAM --> CALL-TABLE
% mk_call_table(Clauses, Call_table, Program)
% Constructs the call_table for the Clauses using the Program
% Each entry of the Call_table is a pair of the form
% <p, [q1, .. , qn]> where the predicate SYMBOL p derives q1,.. qn
% "p DERIVES q" is the transitive closure of "p CALLS q".
% The table has one such entry for each p defined in the Program.

mk_call_table([],_T,_Prog).

% rules 
mk_call_table([:-(H,_B)|Cl], T, Prog) :-
		H =.. [P|_],
		in_table(P,T,_),
		mk_call_table(Cl, T, Prog).

mk_call_table([:-(H,_B)|Cl],T, Prog) :-
		H =.. [P|_],
		derives(P,Plist,Prog,[],[]),
		insert_table(P,Plist,T),
		mk_call_table(Cl,T,Prog).


% facts
mk_call_table([H|Cl], T, Prog) :-
		H =.. [P|_],
		in_table(P,T,_),
		mk_call_table(Cl, T, Prog).

mk_call_table([H|Cl], T, Prog) :-
		H =.. [P|_],
		derives(P,Plist,Prog,[],[]),
		insert_table(P,Plist,T),
		mk_call_table(Cl,T,Prog).


% in_table(Pred, Table, List)  pair(Pred,List) is in the Table

in_table(_P, T, _L) :- var(T),!, fail.
in_table(P,[pair(P,L)|_],L).
in_table(P,[pair(_Q,_)|T],L) :-
		in_table(P,T,L).

% insert_table(Pred,  List, Table)  pair(Pred,List) is in the Table

insert_table(P, L, T) :- var(T),!, T = [pair(P,L)|_].
insert_table(P,[pair(P,L)|_],L).
insert_table(P,L,[pair(_Q,_)|T]) :-
		insert_table(P,L,T).



% derives(P,L,Prog,Lseen,Pseen): The list L 'derives' P in Prog.
%			   Lseen is the list of clauses seen so far.
derives(_P,[],[],_Lseen,_Pseen).

derives(P,L,[C|Cl],Lseen,Pseen) :- 	
		head_match(P,C,B), 
		pred_list(B,L1),
		derives(P,L2,Cl,[C|Lseen],Pseen),
		append(L1,L2,L3),
		append(Cl,Lseen,L4),
		derives_list(B,L5,L4,[C],[P|Pseen]),
		append(L3,L5,L6),
		rm_duplicates(L6,L).

derives(P,L,[C|Cl],Lseen,Pseen) :- 
		derives(P,L,Cl,[C|Lseen],Pseen).

% derives_list(Bl,L,Prog,Lseen,Pseen): list L sans Pseen derives the list Bl
% Pseen, Lseen are  used to avoid cycles 
derives_list([],[],_Prog,_Lseen,_Pseen).

derives_list([H|T],L,Prog,Lseen,Pseen) :- 
		H =.. [HP|_], 
		member(HP,Pseen),
		derives_list(T,L,Prog,Lseen,Pseen).
		
derives_list([H|T],L,Prog,Lseen,Pseen) :- 
		H =.. [HP|_], 
		derives(HP,L1,Prog,Lseen,Pseen),	
		derives_list(T,L2,Prog,Lseen,[HP|Pseen]),
		append(L1,L2,L).
	
	


% head_match(P,C,Bl): The clause C is "P(..) :- Bl."
head_match(P,:-(H,B),Bl) :- 
		H =.. [P|_],
		mk_list([B],Bl).



% pred_list(Bl,L): The list of predicate symbols in the list Bl is L.
pred_list([],[]).
pred_list([B|Bl],[H|L]) :-
		B =.. [H|_],
		pred_list(Bl,L).

% member
member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

% append : Predefined in Quintus Prolog
append([],L,L).
append([X|XL],Y,[X|Z]) :- append(XL,Y,Z).

% rm_duplicates(L1,L2) removes duplicates from L1 to get L2.

rm_duplicates([],[]).
rm_duplicates([H|T],L) :- 
		member(H,T), rm_duplicates(T,L).
rm_duplicates([H|T], [H|L]) :-
		rm_duplicates(T,L).

