%================================================
%               MESSAGGISTICA DI HELP
%               M.O.  08
%===============================================
:- module(gen_msg, [genh/0, crh/0, hh/1]).


crh :-
	working_directory(D,D),
	concat(D,'\\types\\chk.h', F),
        show(F).


hgen1 :-
writeln(
'Dopo aver creato il SIGNATURE OUT FILE con nome XXX_out.pl oppure out.pl,
 per attivare la generazione si carica tale file con:'),nl,
writeln('---> gen(XXX_out)   caricare XXX_out.pl').

genh :-
hgen1,nl,
writeln(
'Per vedere i comandi di generazione, usare:

---> hh(sign)    per rivedere la segnatura
---> hh(terms)   per la generazione di termini
---> hh(atomic)  per la generazione di atomiche
---> hh(prog)	 per la parte di generazione relativa al programma
---> hh(gen)     per rivedere l''help su come caricare i files
---> hh(X)       per tutte
---> crh         help type checker
---> genh	 questo stesso help'),nl.

hh(hgen) :-
nl,writeln('CARICAMENTO SEGNATURA:'),nl,
	hgen1,nl,
	writeln('*** digita ; per continuare; per altri help:  crh e genh').

hh(sign) :-
nl,writeln('SEGNATURA:'),nl,
writeln(
'---> is_type(?T:term).        enumera le dichiarazioni di tipo
---> is_fun(?F:term).         enumera le dichiarazioni di funzione
---> is_pred(?P:pred).        enumera le dichiarazioni di predicato'),nl,
writeln('*** digita ; per continuare; per altri help:  crh e genh').

hh(terms) :-
nl,writeln('TERMINI:'),nl,
writeln(
'---> is_of(+T:Term, -Ty:Type).	         trova il tipo Ty di T'),
writeln(
'---> is_term(-T:Term, ?Ty:Type, +H:Int).  elenca i T di tipo Ty
					 e altezza <= H'),
writeln(
'---> g_term(?T:Term, ?Ty:Type, +H:Int).  elenca i T ground di tipo Ty
                                         e altezza <= H'),
writeln('*** digita ; per continuare; per altri help:  crh e genh').


hh(atomic) :-
nl,writeln('ATOMICHE:'),nl,
writeln(
'---> is_atomic(+A:term).	  A e'' un''atomica'),
writeln(
'---> g_atomic(?A:term, +H:int).   elenca le atomiche A con termini
				  di altezza <=H'),
writeln('FINE, per rivedere l''help usare hh(<argomento>) o hh(X);
         altri help: crh  e  genh').

hh(prog) :-
nl,writeln('PROGRAMMA:'),nl,
writeln(
'---> is_clause(?H:term, ?B:body).   H :- B e'' una clausola'),
writeln(
'---> show_tp(+H:int).               stampa il modello ottenuto con tp
				     e H iterazioni'),
writeln(
'---> g_success_atom(?A,+H:int).    A e'' un atomo ground di altezza max H
	                            e :- A ha successo'),
writeln('FINE, per rivedere l''help usare hh(<argomento>) o hh(X);
         altri help: crh  e  genh').




show_list(L) :-
	maplist(show,L).
show(F) :-
        open(F,read,FF),
	copy_stream_data(FF,user_output),
	close(FF).
















