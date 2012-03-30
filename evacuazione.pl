%Prima versione del progetto di Intelligenza artificiale
%
:- use_module('types/chk').

%classico tipo liste
type list(El) -->[];[El|list(El)].

%per ora ho pensato di mettere solo i nomi incrocio delegando a strada
%i collegamenti tra incroci e i pesi
%per ora ho messo solo 6 incroci. Aumentabile a piacere.

type incrocio -->incrocio1;incrocio2;incrocio3;incrocio4;incrocio5;incrocio6.

%tipo strada->st(incrocio,incrocio collegato,peso della strada)
type strada --> st(incrocio,incrocio,int).

%il tipo stato definisce la posizione sugli incroci di OGNI gruppo
%provvisorio: solo un gruppo!!
type stato -->in(incrocio).

pred sicuro(incrocio).

pred agibile(strada).

%il predicato trafficata non so se sia necessario
pred trafficata(strada).
