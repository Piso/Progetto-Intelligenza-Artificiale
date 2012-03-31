%%%%%%   Adattatore di Typed Prolog,   Mario Ornaghi,  ott 2007
%

% ----------------------------------------------------------------------
% * Loads a file:  
% 	a. Loads the declarations for (T)ypes,(F)unctions and (P)redicates
% and	b. Loads the Program clauses.
% * Checks if the program is well-formed (well_formed.p) w.r.t. T,F and P.
% * If P is not completely specified, reconstructs the P (if possible) 
%   that makes the program well-formed.
% * If verbose, prints the TYPES  T,F and P and the program
% * Writes the TYPES  T,F and P and the program on to : 
% * 	a. "file_oup.pl" if a single file name was specified as input
% *	b. "output.pl" if a list of files were specified as input
% Bug fix: 4/20/93 (thanks to Florence Pagani) in defn of type-preserving
% ----------------------------------------------------------------------


:- module(checker, [rload/0, rload/1,
		   op(1199, fx, [(type), (pred)]),
		   op(1110, xfy, (-->)),
		   crh/0,
		   mk_verbose/0,
		   mk_silent/0,
		   gen/1,
		   genh/0,
		   hh/1, 
		   is_of/2,
		   a_term/3,
		   g_term/3,
		   is_atomic/1,
		   g_atomic/2,
		   is_fun/1,
		   is_type/1,
		   is_pred/1,
                   no_check/1
		   ]).
:-['types_mod/cr_io.pl'].
:-['types_mod/well_formed.pl'].
:-['types_mod/calltable.pl'].
:-['types_mod/adj_list.pl'].
:-['types_mod/order_program.pl']. 
:-['types_mod/auxo.pl'].
:-['types_mod/genera.pl'].
	
no_check(X) :- mk_external(X).
:- 
  writeln('------------------------------------------------------------------'),
  writeln(' Typed Prolog: TYPE CHECKING  /  TYPE RECONSTRUCTION   version 1.0'),
  writeln(' (c) T.K. Lakshman  University of Illinois @ Urbana-Champaign 1990'),
  writeln('------------------------------------------------------------------'),
  nl,nl,
  writeln('Per eseguire l''algoritmo di Type Checking / Reconstruction, usare:'), nl,
  writeln( 
'---> rload.			caricamento interattivo di 1 solo file
---> load(<name>).		caricamento del file <name>.pl
---> load([<name>,..,<name>]).	caricamento della lista di file <name>.pl,... \n
---> crh.			help da usare per vedere le altre opzioni
---> genh                       help per l''AMBIENTE DI GENERAZIONE'),nl.

crh :- 
writeln( 
'---> rload.			caricamento interattivo di 1 solo file
---> load(<name>).		caricamento del file <name>.pl
---> load([<name>,..,<name>]).	caricamento della lista di file <name>.pl,... 
---> crh.			help'),
write(
'Altre opzioni e comandi:
---> mk_verbose.		modo "verbose" (stampa segnatura e programma) 
---> mk_silent.			disabilita "verbose"
---> gen(FileName).		carica XXX_out.pl o out.pl file per la
                                visualizzazione di atomi e termini, 
				per saperne di piu'' usare genh:
---> genh.			help per la GENERAZIONE
---> crh.			questo stesso help').











