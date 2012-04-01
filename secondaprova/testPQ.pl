%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST CASES
:- consult(priorityQ).

bil(o,0).
bil(a(_,A,B), N) :-
   bil(A,NA),
   bil(B,NB),
   (   NA =:= NB ; NA =:= NB-1 ),
   N is NA+NB+1.

perm([],[]).
perm(P,[X|Q]) :-
  extr(X,P,U),
  perm(U,Q).

extr(X,[X|R],R).
extr(X,[T|R],[T|S]) :-
     extr(X,R,S).

aggiunta([],A,A,_).
aggiunta([X|L],A,B,Leq) :-
	ins(X,A,A1,Leq),
	aggiunta(L,A1,B,Leq).

test1 :-
       between(1,6,I),
       buildperm(X,I),
       aggiunta(X,o,A,<),
       checkbil(A,I),
       exttest(A,o,Y),
       checkord(Y,<),
       fail.

checkbil(A,I) :-
       bil(A,I) -> writeln(bilanciamentoOK);
                   throw(errore_bilanciamento(A)).
isord([],_).
isord([_],_).
isord([X,Y|R],Leq) :-
      call(Leq,X,Y),
      isord([Y|R],Leq).
checkord(X,Leq) :-
      isord(X,Leq) -> writeln(ordinamentoOK);
                  throw(errore_ordinamento(X)).

buildperm(P,K) :-
   setof(I, between(1,K,I), P1),
   perm(P1,P).


exttest(o, o, []) :- !.
exttest(F,F1, [N|E]) :-
     extract(N,F,F2,<),
     exttest(F2,F1,E).

exttest0(o, o) :- !.
exttest0(F,F1) :-
     extract(_N,F,F2,<),
     exttest0(F2,F1).


ii(0,A,A) :- !.
ii(K,A,B) :-
	H is K-1,
	random(X),
	I is round(100*X),
	ins(I,A,A1,<),
	ii(H,A1,B).

test2a(K) :-
     time(ii(K,o,A)),
     writeln('***** checking bil ***'),
     writeln('***** Extracting to a list ***'),
     time(exttest0(A,o)),
     writeln('************  Extracted ******').

test2b(K) :-
     ii(K,o,A),
     bil(A,I),
     writeln(A+I),
     exttest(A,o,Y),
     writeln(Y),
     checkord(Y,<).
