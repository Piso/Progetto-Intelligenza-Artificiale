%% Strategia scelta
:- consult(best_first).

%%	dominio problema
:- consult(commesso).





%%	Interfaccia utente

%%	Spiegare all'utente cosa puo' fare con questa interfaccia
%
:- nl, nl, writeln('COMANDI  =========================================='),
	nl,
	writeln('show_piano(R:robot, Ordini:list(articolo)'),
	writeln('con articolo --> art(num)'),
	 nl,
	 writeln('================================================'),
	 nl.

%%	calcolo il piano reltivo agli Ordini in ingresso e lo mostro in
%	output

show_piano(Robot,Ordini) :-
	piano(Robot,Ordini,Piano,Costo),
	maplist(write,['Piano di costo ', Costo, ':\n']),
	maplist(writeln,Piano).

%%	Utile prevedere ispezione base dati, da FARE


/*****   chiamata dell'algoritmo di ricerca per il
         calcolo del piano utilizzando l'input dell'interfaccia
	 ******************/

%	piano(+R:robot, +Ordini:list(articolo), -Piano:list(azione),
%	-Costo:real)
%	Piano = sequenza di a
%

piano(Robot,Ordini, Piano, Costo) :-
	maplist(ord(Robot), Ordini, OrdR),  % costruisco la lista di
                                            % ordini per Robot
	solve(st(Robot,init,OrdR),nc(G,L,Costo)), % lancio l'algoritmo di ricerca

	reverse([G|L],RL),		    %estraggo il piano dalla lista
	maplist(extract_act, RL, Piano).    % [G|L]

extract_act(st(_,Azione,_), Azione).  %estrae l'azione
ord(Robot,Articolo, ordine(Robot,Articolo)).  % costruisce l'ordine per Robot






