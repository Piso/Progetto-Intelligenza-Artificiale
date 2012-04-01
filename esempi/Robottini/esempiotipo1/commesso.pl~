:- use_module('types/chk').

:- no_check(setof(_,_,_)).

type num --> 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15;
             16; 17; 18; 19; 20.
type articolo --> art(num).
type armadio --> a1; a2; a3; a4.
type robot --> roby.
type luogo --> banco; a(armadio).

type fluent --> posizione(robot,luogo);
                porta(robot,articolo);
		ordine(robot,articolo).
type azione --> init;
                va(luogo,luogo);
                prende(articolo).
type list(X) --> []; [X|list(X)].

type stato --> st(robot,azione,list(fluent)).
                % Stati dello spazio degli stati.
                % Significato di st(Robo,Act,S):  Robo è appena arrivato nello
                % stato-fluenti S avendo appena eseguito l'azione Act

pred contiene(armadio,articolo).
%  modi contiene(?A, ?Art), è un predicato di ggrounding
%  significato: "A contiene Art " (non badiamo alle quantità)

pred percorso(luogo,luogo,num).
%  modi percorso(?L1,?L2,?Lung), è un predicato di grounding
%  significato: il percorso minimo da L1 a L2 è lungo Lung
%  Definisco percorso simmetrico in base alla distanza fra
%  due luoghi, definita in una base dati asimmetrica per
%  evitare loop

percorso(A,B,C) :- dist(A,B,C); dist(B,A,C).



%%	archi dallo stato iniziale:  il robot ha acquisito gli ordini;
%       va ad un armadio; NB:  list_to_ord_set perchè userò
%       gli ordset
%
arc(st(R,init,Ordini), st(R,va(banco,L),S)) :-
	percorso(banco,L,_),
	list_to_ord_set([posizione(R,L)|Ordini],S).


%%	.... COMPLETARE con gli archi da stato intermedio a stato
%	intermedio


%%	stato finale:  non ci sono più ordinazioni da prendere,
%       il robot va al banco a consegnare
%
arc(st(R,prende(_),S1),st(R,va(L,banco),S2)) :-
	percorso(L,banco,_),
	ord_memberchk(posizione(R,L),S1),
	not(ord_member(ordine(R,_),S1)),   % non ci sono ordinazioni pendenti
	ord_subtract(S1,[posizione(R,L)],S),
	ord_union([S,[posizione(R,banco)]], S2).

%%	vicini, trovato, costo

%%	... COMPLETARE definendo vicini, trovato e costo; qui per
%	trovato si noti che lo stato finale si raggiunge solo
%	eseguendo l'azione va(L,banco)  da un luogo al banco


%%  Il taglio cicli ora utilizza eq(stato,stato) come eguaglianza di
%%  stati. Dal momento che lo stato-fluenti S è una lista
%%  ordinata ground, l'uguaglianza insiemistica coincide con l'identità;
%%  quindi se trascuriamo l'azione attraverso la quale si raggiunge uno
%%  stato e il robot che la esegue (un dubbio; se si passa a più robot,
%%  è giusto trascurare il robot nella eguaglianza fra stati usata per
%%  tagliare lo spazio di ricerca? non ci ho pensato, pensateci voi)
%%  possiamo definire eq come segue

eq(st(_,_,S), st(_,_,S)).



%%	DARE LA BASE DATI CHE RAPPRESENTA UNO SPECIFICO NEGOZIO
%	merdiante dist e contiene






























