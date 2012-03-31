% ----------------------------------------------------------------------
% 	Typed Prolog: Type Checking & Reconstruction (REORDER THE PRED DEFNS)
% 	Created by T.K. Lakshman, University of Illinois, Feb 20, 1991.
% ----------------------------------------------------------------------
% ADJACENCY-LIST X PROGRAM --> ORDERED-PROGRAM (TOPOLOGICAL SORT)
% ----------------------------------------------------------------------
order_program(Adj_list,Program,NewProgram) :-
% 1. replace maximal strongly connected components by a new vertex
		reduce_graph(Adj_list,Reduced_Adj_list,Cycles),
% 2. Get the list of predicates being DEFINED		
		get_defns(Reduced_Adj_list,L),	
% 3. topological-sort the (ACYCLIC) Reduced_Adj_list
		top_sort(L,Reduced_Adj_list,SortL,_Visited),
% 4. Replace vertices by their associated cycles
		expand(SortL,NewL,Cycles),
% 5. Reorder the program using NewL
		reorder_prog(NewL,Program,NewProgram).	


% 1. replace maximal strongly connected components by  new vertices
% Algorithm due to Kosaraju and Sharir (see Aho/Hopcroft/Ullman '83)
% reduce_graph(Adj_list,Reduced_Adj_list,Cycles)  O(|v| + |e|)
reduce_graph(Adj_list,Reduced_Adj_list,Cycles) :-
		get_defns(Adj_list,L),
		top_sort(L,Adj_list,L1,_Visited),
		reverse_list(L1,L2),
		transpose(Adj_list,T_Adj_list),
		dfs(L2,T_Adj_list,_LCycles,_T_Visited,Cycles), 
% LCycles is a list of Lists. Cycles is a table of pairs(P,PCycle)
		mk_reduced_graph(Adj_list,Cycles,Reduced_Adj_list).

dfs(L,_T_Adj_list,[],_T_Visited,_Cycles) :- var(L).
dfs([],_T_Adj_list,[],_T_Visited,_Cycles).
dfs([P|L],T_Adj_list,LCycles,T_Visited,Cycles) :-
		in_list(P,T_Visited),
		dfs(L,T_Adj_list,LCycles,T_Visited,Cycles).
dfs([P|L],T_Adj_list,[PCycle|LCycles],T_Visited,[pair(P,PCycle)|Cycles]) :-
		dfs_visit(P,T_Adj_list,PCycle,T_Visited),
		dfs(L,T_Adj_list,LCycles,T_Visited,Cycles).

dfs_visit(P,T_Adj_list,PCycle,T_Visited) :-
		insert(P,T_Visited),
		get_list(P,PL,T_Adj_list),
		dfs_visit_List(PL,T_Adj_list,L1,T_Visited),
		append([P],L1,PCycle).	% P's cycle

dfs_visit_List(L,_T_Adj_list,[],_T_Visited) :- var(L).
dfs_visit_List([],_T_Adj_list,[],_T_Visited).
dfs_visit_List([P|L],T_Adj_list,PCycle,T_Visited) :-
		in_list(P,T_Visited),
		dfs_visit_List(L,T_Adj_list,PCycle,T_Visited).
dfs_visit_List([P|L],T_Adj_list,Cycle,T_Visited) :-
		dfs_visit(P,T_Adj_list,PCycle,T_Visited),
		dfs_visit_List(L,T_Adj_list,LCycle,T_Visited),
		append(PCycle,LCycle,Cycle).
	
% mk_reduced_graph(Adj_list,Cycles,Reduced_Adj_list)
% in adj_list.p		

% transpose(A1,A2) : The transpose of graph (given by A1) is given by A2.
% defined in Adj_list.p

% 2. Get a list of predicates being DEFINED		
get_defns(L,[]) :- var(L).
get_defns([pair(P,_)|L],[P|L1]) :-	
		get_defns(L,L1).

% 3. top_sort(L,Adj_list,NewL,Visited) 
% The standard depth first traversal of  the DAG O(|v| + |e|)
top_sort(L,_,[],_) :- var(L).
top_sort([],_,[],_).
top_sort([P|L],Adj_list,NewL,Visited) :-
		in_list(P,Visited),
		top_sort(L,Adj_list,NewL,Visited).

top_sort([P|L],Adj_list,NewL,Visited) :-
		insert(P,Visited),
		get_list(P,PL,Adj_list),
		top_sort(PL,Adj_list,L1,Visited),
		top_sort(L,Adj_list,L2,Visited),
		append(L1,[P|L2],NewL).

in_list(_P,Visited) :- var(Visited), !, fail.
in_list(P,[P|_]).
in_list(P,[_Q|L]) :- in_list(P,L).

insert(P,[P|_]).
insert(P,[_Q|L]) :- insert(P,L).

get_list(_P,_PL,L) :- var(L).
get_list(P,PL,[pair(P,PL)|_]).
get_list(P,PL,[pair(_Q,_)|L]) :- get_list(P,PL,L).

		
% 4. expand(List,NewList,Cycles)
% Replaces each element of List by the cycle (in Cycles) it's associated with 

expand(L,[],_Cycles) :- var(L).
expand([],[],_Cycles).
expand([P|Pl],List,Cycles) :-
		get_cycles(P,LP,Cycles),
		expand(Pl,LPl,Cycles),
		append(LP,LPl,List).

get_cycles(P,[P],L) :- var(L).
get_cycles(P,LP,[pair(P,LP)|_]).
get_cycles(P,LP,[_|L]) :-
		get_cycles(P,LP,L).


% 5. reorder_prog(Pred_list,Prog,NewProg) 

reorder_prog([],_Prog,[]).
reorder_prog([P|PL],Prog,NewProg) :-
	get_clauses(P,Prog,Prog1),
	reorder_prog(PL,Prog,Prog2),
	append(Prog1,Prog2,NewProg).

get_clauses(_P,[],[]).

% rules
get_clauses(P,[:-(H,B)|Cl],[:-(H,B)|Prog]) :-
		H =.. [P|_],
		get_clauses(P,Cl,Prog).
get_clauses(P,[:-(_,_)|Cl],Prog) :-
		get_clauses(P,Cl,Prog).

% facts
get_clauses(P,[H|Cl],[H|Prog]) :-
		H =..[P|_],	
		get_clauses(P,Cl,Prog).
get_clauses(P,[_H|Cl],Prog) :-
		get_clauses(P,Cl,Prog).

% ----------------------------------------------------------------------
% auxiliary list manipulation fns

reverse_list([],[]).
reverse_list([X|L1],L2) :- reverse_list(L1,L3), append(L3, [X], L2).


% ----------------------------------------------------------------------

