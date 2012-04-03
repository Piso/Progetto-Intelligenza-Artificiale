%=============================================================
%
%     GENERAZIONE DI ESEMPI.   M.O. 08
%
%===============================================================

:- module(genera, [gen/1,
		   is_type/1,
		   is_fun/1,
		   is_pred/1,
		   is_of/2,
		   is_term/3,
		   g_term/3,
		   is_atomic/1,
		   g_atomic/2,
		   is_clause/2,
		   show_tp/1,
		   g_success_atom/2]).

:- consult(gen_msg).
gen(X) :- consult(X).


try(A) :-
	catch(A,E,warn(E)).

:- dynamic(tried/0).
warn(E) :-
	not(var(E)),
	(   tried ->
	        fail
	        ;
	        assert(tried),
	        writeln('HAI ESEGUITO gen(...)?'),
	        fail
	).

%************************* a type
%

is_type(T) :- try(rtype(T)).

% *************************   a_funct
% SPEC:  a_funct(var X, ?N:int, ?Types:list(a_type), ?T:a_type)
%	 per ogni  f: t1 x ... x tn -> t dichiarata restituisce:
%	 X = f(V1,..,Vn) con variabili fresche V1,..,Vn
%	 N = n
%	 Types = [t1,..,tn],
%	 T = t
%	 ---> EXCEPTION se X non e' una variabile in chiamata

a_funct(X,N,Types,T) :-
	var(X),
	try(rfun(F,Types,T)),
	length(Types,N),
	length(Args,N),
	X =.. [F|Args].
a_funct(X,_,_,_) :-
	not(var(X)),
	throw(a_funct_error(X, deveEssereUnaVariabile)).

is_fun(F:A->T):- try(rfun(F,A,T)).

%%  **************************  a_pred
% SPEC:   a_pred(X:var,?N:int, ?Types)  :   a_pred(X) restituisce
%           X = p(_X1,..,_Xn), N=n, Types = [t1,..,tn]
%	    per ogni predicato p(t1,..,tn) dichiarato

a_pred(X,N,Types) :-
	var(X),
	try(rpred(P,Types)),
	length(Types,N),
	length(Args,N),
	X =.. [P|Args].
a_pred(X,_,_) :-
	not(var(X)),
	throw(a_pred_error(X, deveEssereUnaVariabile)).

is_pred(P:A->(o)):- try(rpred(P,A)).


%**********************************************************************+***************************
%********************************************   TERMINI  **********

%% ****************************  is_of
% SPEC:  +X:term is_of -T:a_type :   T e' il tipo di X

is_of(X,_) :-
	var(X),!.
is_of(X,T) :-
	a_funct(F,_N,Types,T),
	unifies(X,F,_,Args),
	areOf(Args,Types).
areOf([],[]).
areOf([X|L],[Y|M]) :-
	is_of(X,Y),
	areOf(L,M).


%% *******************************  is_term
%% SPEC:    is_term(?X:term, +T_a_type,+N:int) : X e' un termine di tipo T
%%                                        di profondita' al piu' N
is_term(var(_),_T,_).
is_term(X,T,H) :-
	a_funct(F,_N,Types,T),
        unifies(X,F,_,Args),
	prec(K,H),
	terms(Args,Types,K).

terms([],[],_).
terms([T|A],[TT|TA],K) :-
	is_term(T,TT,K),
	terms(A,TA,K).

%%	ausiliario



%% *******************************  g_term
%% SPEC:    g_term(?X:term, +T_a_type,+N:int) : X e' un termine ground di
%                                         tipo T di profondita' al piu' N
%%

g_term(X,T,H) :-
	a_funct(F,_N,Types,T),
	unifies(X,F,_,Args),
	prec(K,H),
	gterms(Args,Types,K).

gterms([],[],_).
gterms([T|A],[TT|TA],K) :-
	g_term(T,TT,K),
	gterms(A,TA,K).


% ********************************************************************************************************
%********************************FORMULE ATOMICHE ******************

%% *************************** an_atomic
%% SPEC:   an_atomic(+A) :  A e' un'atomica ben tipato

is_atomic(A) :-
    a_pred(P,_N,Types),
    unifies(A,P,_,Args),
    areOf(Args,Types).

%% ***************************  g_atomic
%%	   g_atomic(?A,+H:int) :  A e' un'atomica ground ben tipato
%%	                        con termini di profondita'
%%	                        massima H

g_atomic(A,H) :-
    a_pred(P,_N,Types),
    unifies(A,P,_,Args),
    gterms(Args,Types,H).



%%	****************************************************** CLAUSOLE
%       usiamo la meta-programmazione per ottenere le clausole del programma corrente

%********************************  is_clause
% SPEC:  is_clause(?P:an_atomic, ?Body:a_body):
%        P e' dichiarato e P :- Body appartiene al programma corrente

is_clause(P, Body):-
	a_pred(P,_N,_Types),
	clause(P,Body).


%************************************************************* CONSEGUENZA IMMEDIATA

% ************************************   vero
% SPEC  vero(+B:body, +L:interpretazione):    L |= B
%    type interpretazione = list(atomic)

vero(true,_) :- !.

vero(X=X,_) :- !.

vero((A;B),L) :- !,
	vero(A,L);
	vero(B,L).
vero((A,B),L) :- !,
	vero(A,L),
	vero(B,L).
vero(A,L) :-
	%not(A=(_,_)),
	%not(A=(_;_)),
	member(A,L).

% **********************************  conseguenza
% SPEC: conseguenza(H,L) :
%	      esiste una clausola  H :- Body  tale che  L |= Body

conseguenza(H,L) :-
	is_clause(H,Body),
	vero(Body,L).

% ******************************** conseguenze
% SPEC:  conseguenze(I,J) :  { H  |  conseguenza(H,I) }

conseguenze(I,J) :-
	findall(H, conseguenza(H,I), J1),
	list_to_ord_set(J1,J).


%%	******************  OPERATORE TP
% SPEC:   tp(-M:interpretazione, +H:int):  chiamato con H=h,
%                                         calcola M = TP^h([])
%					  modello generato in h passi
%                                         prove di altezza massima h

tp(M,0) :- ord_empty(M).
tp(M,H) :- prec(K,H),
	   tp(MK,K),!,
	   conseguenze(MK,M).

show_tp(N) :-
	tp(M,N),
	maplist(writeln,M).


%%	********************  SUCCESS SET
% SPEC:  g_success_atom(-A:g_atomic, +H:int):   A appartiene al modello minimo
%                                          e A ha profondita' max. H

g_success_atom(_A,_H) :-
	writeln('DA FARE').
	/*g_atomic(A,H),
	catch(A, _, fail).*/



%%	%%%%%%%%%%%%%  UTIL

unifies(E1,E2,Functor,Args) :-
	E1 = E2,
	E1 =.. [Functor|Args].

% SPEC:  prec(?P:int,+N:int):   N > 0 e P = N-1
prec(P,N) :-
	N > 0,
	P is N-1.

ins(X,[Y|L]) :- X=Y,!; ins(X,L).

mem(_X,L) :- var(L), !, fail.
mem(X,[Y|L]) :- X=Y; mem(X,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

















