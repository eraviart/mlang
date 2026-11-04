application app;
X : saisie contexte classe = 0 priorite = 0 categorie_TL = 0 modcat = 1 restituee primrest = 0 alias AX : "variable X de test" type REEL ;
Y : calculee restituee primrest = 0 : "";
T : tableau[5] calculee primrest = 0 : "";

regle primitive 1:
application : app;
Y = si (X > 100) alors (arr(X * 2) + 0) sinon (0) finsi ;

verif 1:
application : app;
si (X <= 100) alors erreur ATSTER ;

cible orchestration:
application : app;
calculer domaine primitive;
verifier domaine primitive;
exporte_erreurs;
