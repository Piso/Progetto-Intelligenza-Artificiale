
trovato(d).

vicini(a,[b,c]).
vicini(b,[a,c,d]).
vicini(c,[d,a]).
vicini(d,[a]).

costo(a,b,1).
costo(a,c,3).
costo(b,c,1).
costo(b,a,1).
costo(b,d,1).
costo(c,a,1).
costo(c,d,1). %%%
costo(d,a,1).

