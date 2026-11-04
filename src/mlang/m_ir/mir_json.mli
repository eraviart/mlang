(* F11/2 — sérialiseur MIR → JSON (ticket GitLab #96, fork eraviart/mlang,
   branche f11-serialiseur, base 66f6b373 = release 189 épinglée).

   Sérialise le programme MIR tel qu'exécuté (après expansion des fonctions,
   cf. Driver.driver) vers un JSON compact, avec position source par nœud.
   C'est un produit de LECTURE : aucune sémantique d'exécution n'est ajoutée ;
   l'exécution de contrôle reste l'interpréteur Mlang. *)

val program_to_json : Mir.program -> Yojson.Basic.t
(** Représentation JSON complète du programme MIR. *)

val write_program : string -> Mir.program -> unit
(** Écrit la sérialisation compacte dans le fichier donné. *)
