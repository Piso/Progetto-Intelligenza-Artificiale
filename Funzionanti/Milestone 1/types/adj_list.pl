% ----------------------------------------------------------------------
% 	Typed Prolog: Type Checking & Reconstruction (MAKE ADJACENCY LIST)
% 	Created by T.K. Lakshman, University of Illinois, Feb 20, 1991
% ----------------------------------------------------------------------
% PROGRAM --> ADJACENCY LIST
% mk_adj_list(Program, PList)
% Each entry of the adj_list is a pair of the form
% <p, [q1, .. , qn|_]> where  clause(s)  p <-  q1,.. qn AND (p <> qi)
% i.e. the adj_list of p does NOT contain p.
% The table has one such entry for each p defined in the Program

mk_adj_list([],_PList).

 
% rules 
mk_adj_list([:-(H,B)|Cl], PList) :- 
		H =.. [P|_],
		mk_list([B],Bl),
		pred_list(Bl,L1,P),
		insert_a_list(P,L1,PList),
		mk_adj_list(Cl,PList).
% facts
mk_adj_list([H|Cl],PList) :-
		H =.. [P|_],
		insert_a_list(P,[],PList),
		mk_adj_list(Cl,PList).


% insert_a_list(Pred,  List, PList)  pair(Pred,List) is in the PList

insert_a_list(P, L,[pair(P,L1)|_]) :- 
		insert_list(L,L1).
insert_a_list(P,L,[pair(_Q,_)|PList]) :- 
		insert_a_list(P,L,PList).

insert_list(L,_L1) :- var(L).
insert_list([],_L).
insert_list([H|T],[H|L]) :-
		insert_list(T,L).
insert_list([H|T],[_H1|L]) :-
		insert_list([H|T],L).

% pred_list(Bl,L,H): The list of predicate symbols (other than H)
% in the list Bl is L.
pred_list([],[],_).
pred_list([B|Bl],L,H) :-
		B =.. [H|_],
		pred_list(Bl,L,H).
pred_list([B|Bl],[P|PL],H) :-
		B =.. [P|_],
		pred_list(Bl,PL,H).

% mk_list
mk_list([(A,B)], [A|L]) :- mk_list([B],L).
mk_list([],[]).
mk_list([H|[]],[H|[]]).

% ----------------------------------------------------------------------

	
% transpose of a graph O(|v| + |e|)

transpose(AL,_AL1) :- var(AL).
transpose([pair(P,L)|L1],AL1) :-
		ins_pair(L,[P],AL1),
		transpose(L1,AL1).

ins_pair(L,_LP,_) :- var(L).
ins_pair([H|T],LP,AL1) :-
		insert_a_list(H,LP,AL1),
		ins_pair(T,LP,AL1).

% Graph --> acyclic component Graph
% mk_reduced_graph(Adj_list,Cycles,Reduced_Adj_list)

mk_reduced_graph(L,_Cycles,_Reduced_Adj_list) :- var(L).
mk_reduced_graph([pair(P,PL)|L],Cycles,Reduced_Adj_list) :- 
		rename(P,Cycles,P1),
		rename_list(PL,Cycles,PL1),
		insert_a_list(P1,PL1,Reduced_Adj_list),
		mk_reduced_graph(L,Cycles,Reduced_Adj_list).

rename_list(L,_Cycles,_PL1) :- var(L).
rename_list([P|PL],Cycles,[P1|PL1]) :- 
		rename(P,Cycles,P1),
		rename_list(PL,Cycles,PL1).

rename(P,C,P) :- var(C).
rename(P,[],P).
rename(P,[pair(P,_)|_],P).
rename(P,[pair(Q,QL)|_],Q) :- 
		in_list(P,QL).
rename(P,[_|CL],R) :- rename(P,CL,R).
	
% ----------------------------------------------------------------------



