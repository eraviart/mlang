application app;
X : saisie contexte classe = 0 priorite = 0 categorie_TL = 0 modcat = 1 restituee primrest = 0 alias AX : "variable X de test" type REEL ;
Y : calculee restituee primrest = 0 : "";
Z : calculee restituee primrest = 0 : "";
MODE_CORR : saisie environnement toto = 0 alias EST_CORR : "" ;

cible prim:
application : app;
Y = X + 1;

cible corr:
application : app;
iterer
: variable VX
: categorie saisie contexte
: dans (
  Y = Y + 1;
)
restaurer
: variable VX
: categorie saisie contexte
: apres (
  Y = Y + 2;
)
arranger_evenements
: trier R1, R2
: avec si (champ_evenement(R1, numero) < champ_evenement(R2, numero)) alors (1) sinon (0) finsi
: filtrer E
: avec champ_evenement(E, numero) >= 0
: ajouter 2
: dans (
  iterer
  : variable R
  : entre 0..(nb_evenements() - 1) increment 1
  : dans (
    si champ_evenement(R, numero) = 0 alors
      champ_evenement(R, code) reference Y;
    finsi
  )
)
quand nb_anomalies() = 0 faire
  Y = 1;
puis_quand nb_discordances() = 0 faire
  Z = 1;
sinon_faire
  Z = 0;
finquand

cible orchestration:
application : app;
si MODE_CORR + 0 = 1 alors
  calculer cible corr;
sinon
  calculer cible prim;
finsi
