(* F11/2 — sérialiseur MIR → JSON (ticket GitLab #96, fork eraviart/mlang,
   branche f11-serialiseur, base 66f6b373 = release 189 épinglée).

   Le schéma mire les types de {Com} et {Mir} : chaque forme du langage M
   présente dans la MIR devient un nœud JSON nommé d'après sa construction M
   (positif, arr, present, indefini…), avec sa position source. Les formes
   expandées par le front-end (boucles, somme, fonctions utilisateur,
   affectations génériques) n'apparaissent plus ici : elles sont déjà
   réduites par {M_frontend.Expander} et {Mir.expand_functions}.
   Aucune interprétation : le JSON est une projection fidèle de la MIR ;
   l'exécution de contrôle reste l'interpréteur Mlang. *)

open Com

let json_pos (p : Pos.t) : Yojson.Basic.t =
  let sline, scol = (Pos.get_start_line p, Pos.get_start_column p) in
  let eline, ecol = (Pos.get_end_line p, Pos.get_end_column p) in
  `Assoc
    [
      ("file", (match Pos.get_file p with "" -> `Null | f -> `String f));
      ("sline", `Int sline);
      ("scol", `Int scol);
      ("eline", `Int eline);
      ("ecol", `Int ecol);
    ]

(* Valeur marquée : {value, pos}. *)
let marked (jv : 'a -> Yojson.Basic.t) (m : 'a Pos.marked) : Yojson.Basic.t =
  let v, p = Pos.to_couple m in
  `Assoc [ ("value", jv v); ("pos", json_pos p) ]

let m_pos (m : 'a Pos.marked) : Yojson.Basic.t = json_pos (Pos.get m)

let opt (jv : 'a -> Yojson.Basic.t) (o : 'a option) : Yojson.Basic.t =
  match o with None -> `Null | Some v -> jv v

let opt_marked (jv : 'a -> Yojson.Basic.t) (o : 'a Pos.marked option) :
    Yojson.Basic.t =
  match o with None -> `Null | Some m -> marked jv m

let json_string = fun s -> `String s

let json_int = fun i -> `Int i

let json_float = fun f -> `Float f

let json_str_map (jv : 'a -> Yojson.Basic.t) (m : 'a StrMap.t) :
    Yojson.Basic.t =
  `Assoc (List.map (fun (k, v) -> (k, jv v)) (StrMap.bindings m))

let json_int_map (jv : 'a -> Yojson.Basic.t) (m : 'a IntMap.t) :
    Yojson.Basic.t =
  `List
    (List.map (fun (k, v) -> `Assoc [ ("id", `Int k); ("value", jv v) ])
       (IntMap.bindings m))

let json_literal (l : literal) : Yojson.Basic.t =
  match l with Float f -> `Float f | Undefined -> `String "indefini"

let json_unop (o : unop) : Yojson.Basic.t =
  match o with Not -> `String "non" | Minus -> `String "moins_unaire"

let json_binop (o : binop) : Yojson.Basic.t =
  match o with
  | And -> `String "et"
  | Or -> `String "ou"
  | Add -> `String "add"
  | Sub -> `String "sub"
  | Mul -> `String "mul"
  | Div -> `String "div"
  | Mod -> `String "mod"

let json_comp_op (o : comp_op) : Yojson.Basic.t =
  match o with
  | Gt -> `String "gt"
  | Gte -> `String "gte"
  | Lt -> `String "lt"
  | Lte -> `String "lte"
  | Eq -> `String "eq"
  | Neq -> `String "neq"

let json_func (f : func) : Yojson.Basic.t =
  match f with
  | SumFunc -> `String "somme"
  | AbsFunc -> `String "abs"
  | MinFunc -> `String "min"
  | MaxFunc -> `String "max"
  | GtzFunc -> `String "positif"
  | GtezFunc -> `String "positif_ou_nul"
  | NullFunc -> `String "null"
  | ArrFunc -> `String "arr"
  | InfFunc -> `String "inf"
  | PresentFunc -> `String "present"
  | Multimax -> `String "multimax"
  | Supzero -> `String "supzero"
  | VerifNumber -> `String "numero_verif"
  | ComplNumber -> `String "numero_compl"
  | NbEvents -> `String "nb_evenements"
  | Func s -> `String ("fonction:" ^ s)

let json_value_typ (t : value_typ) : Yojson.Basic.t =
  match t with
  | Boolean -> `String "booleen"
  | DateYear -> `String "date_aaaa"
  | DateDayMonthYear -> `String "date_jjmmaaaa"
  | DateMonth -> `String "date_mm"
  | Integer -> `String "entier"
  | Real -> `String "reel"

let json_cat (c : CatVar.t) : Yojson.Basic.t =
  match c with
  | Input subs ->
      `Assoc
        [
          ("kind", `String "input");
          ("sub", `List (List.map (fun s -> `String s) (StrSet.elements subs)));
        ]
  | Computed { is_base } ->
      `Assoc [ ("kind", `String "computed"); ("is_base", `Bool is_base) ]

(* Identifiant de domaine : ensemble ordonné d'étiquettes, joint par espaces.
   C'est aussi la clé utilisée par DomainIdMap. *)
let domain_id_str (id : DomainId.t) : string =
  String.concat " " (DomainId.elements id)

let json_domain_id (id : DomainId.t) : Yojson.Basic.t =
  `String (domain_id_str id)

let json_domain_id_set (s : DomainIdSet.t) : Yojson.Basic.t =
  `List (List.map json_domain_id (DomainIdSet.elements s))

let json_cat_loc (l : CatVar.loc) : Yojson.Basic.t =
  match l with
  | LocComputed -> `String "computed"
  | LocBase -> `String "base"
  | LocInput -> `String "input"

let json_var_ref (v : Var.t) : Yojson.Basic.t =
  `Assoc [ ("id", `Int v.id); ("name", `String (Pos.unmark (Var.name v))) ]

let json_event_field (name : string) (ef : event_field) : Yojson.Basic.t =
  let n, npos = Pos.to_couple ef.name in
  `Assoc
    [
      ("name", `String name);
      ("field", `String n);
      ("pos", json_pos npos);
      ("index", `Int ef.index);
      ("is_var", `Bool ef.is_var);
    ]

(* ---------------------------------------------------------------------- *)
(* F11/A1 (#101) — phases correctif et rappels                            *)
(*                                                                        *)
(* Les instructions de la phase correctif et des rappels étaient déjà      *)
(* projetées par #96 (quand_puis_quand, arranger_evenements, restaurer,    *)
(* champ_evenement, iterer…). Ce bloc ajoute leur *résolution* explicite,  *)
(* c'est-à-dire ce que l'interpréteur Mlang calcule à l'exécution :        *)
(*                                                                        *)
(*  - ref_frames : la machinerie de références empilées. Chaque cible a    *)
(*    une frame de taille target_nb_refs (mast_to_mir.ml:90-91) que        *)
(*    l'interpréteur empile à l'entrée (ctx_ref_org += target_nb_refs,     *)
(*    mir_interpreter.ml:1294-1296) et dépile à la sortie ; un accès de    *)
(*    référence lit ctx_ref.(ctx_ref_org + loc_idx) (mir_interpreter.ml:   *)
(*    368-369) et set_var_ref réécrit cet emplacement à chaque liaison     *)
(*    (mir_interpreter.ml:414-421). D'où les indices négatifs : les        *)
(*    arguments occupent [-nb_refs, -nb_refs+nb_args-1] (mast_to_mir.ml:   *)
(*    227-241) et les références d'itération/restauration [-nbRef, -1]     *)
(*    (mast_to_mir.ml:1036-1037, 876-879) — ils sont comptés depuis la     *)
(*    base *empilée*, donc négatifs. La section donne, par cible, la       *)
(*    taille de frame et, par emplacement (loc_idx), les liaisons qui      *)
(*    l'écrivent (genre + position) : un emplacement peut être réécrit par  *)
(*    plusieurs `iterer`/`restaurer` frères, c'est la pile.                *)
(*  - evenements : les événements déclaratifs. La déclaration `evenement`   *)
(*    (dgfip_m.ml:239-250 : numero, rappel, code, montant, sens, penalite,  *)
(*    base_tl, date, 2042_rect, anc_penalite, id_evt, strate) donne les     *)
(*    champs (index + est_variable) ; l'interpréteur tient une pile         *)
(*    ctx_events (mir_interpreter.ml:69) qu'arranger_evenements empile      *)
(*    après filtre et tri (mir_interpreter.ml:1067-1146), que               *)
(*    champ_evenement lit (FieldAccess, mir_interpreter.ml:445-455), que    *)
(*    nb_evenements compte (mir_interpreter.ml:778-780) et dont             *)
(*    l'affectation de champ (EventFieldRef) lie une variable              *)
(*    (mir_interpreter.ml:855-870).                                         *)
(*  - phases : la phase correctif est la branche « si MODE_CORR + 0 = 1 »   *)
(*    de la cible principale (m_ext/<année>/main.m:22 ; le sélecteur est    *)
(*    lu par mir_interpreter.ml:423-431) ; les rappels sont les cibles qui  *)
(*    manipulent les événements.                                           *)
(*                                                                        *)
(* Rétrocompatibilité : ces sections ne sont émises que si le programme     *)
(* porte cette branche (format_version 2). Les programmes primitifs         *)
(* 2020/2021 (m_ext/2020 et 2021/cibles.m, sans MODE_CORR) restent au       *)
(* format 1, octet pour octet — c'est la preuve de rétrocompatibilité.      *)
(* ---------------------------------------------------------------------- *)

(* État de sérialisation : vrai pendant l'émission d'un document format 2. *)
let annoter_phases = ref false

(* var id → (cible, genre de la liaison, position de la liaison) : résout    *)
(* chaque variable de référence vers la frame et l'emplacement qui la       *)
(* lient (rempli par program_to_json avant l'émission de vars_by_id).       *)
let cadre_des_refs : (int, string * string * Pos.t) Hashtbl.t =
  Hashtbl.create 1024

(* Champs supplémentaires réservés au format 2 : rien n'est émis en v1. *)
let ann (l : (string * Yojson.Basic.t) list) : (string * Yojson.Basic.t) list =
  if !annoter_phases then l else []

let nom_brut (v : Var.t) : string = Pos.unmark (Var.name v)

let est_selecteur_correctif (v : Var.t) : bool = nom_brut v = "MODE_CORR"

let mentionne_selecteur (m_e : Var.t m_expression) : bool =
  Com.m_expr_fold_var
    (fun _usage _sp v_opt acc ->
      acc
      || (match v_opt with
         | Some v -> est_selecteur_correctif v
         | None -> false))
    m_e false

(* Condition « MODE_CORR + 0 = 1 » : littéral comparé (1. → alors, 0. → sinon). *)
let litteral_compare (m_e : Var.t m_expression) : float option =
  match Pos.unmark m_e with
  | Com.Comparison (m_op, _, m_rhs) -> (
      match (Pos.unmark m_op, Pos.unmark m_rhs) with
      | Com.Eq, Com.Literal (Com.Float f) -> Some f
      | Com.Neq, Com.Literal (Com.Float f) -> Some (1. -. f)
      | _ -> None)
  | _ -> None

(* Visiteur minimal (instructions + expressions) : comptes et liaisons de la *)
(* phase correctif ; ne modifie pas la MIR.                                  *)
type visite = {
  vi_instr : (Var.t, Error.t) m_instruction -> unit;
  vi_expr : Var.t m_expression -> unit;
}

let rec visite_expr (vis : visite) (m_e : Var.t m_expression) : unit =
  vis.vi_expr m_e;
  match Pos.unmark m_e with
  | Com.TestInSet (_, e0, values) ->
      visite_expr vis e0;
      List.iter
        (function
          | Com.FloatValue _ | Com.IntervalValue _ -> ()
          | Com.VarValue m_a -> visite_acces vis (Pos.unmark m_a))
        values
  | Com.Unop (_, e0) -> visite_expr vis e0
  | Com.Comparison (_, e0, e1) | Com.Binop (_, e0, e1) ->
      visite_expr vis e0;
      visite_expr vis e1
  | Com.Conditional (e0, e1, e2) ->
      visite_expr vis e0;
      visite_expr vis e1;
      (match e2 with Some e -> visite_expr vis e | None -> ())
  | Com.FuncCall (_, el) -> List.iter (visite_expr vis) el
  | Com.FuncCallLoop (_, _, e0) | Com.Loop (_, e0) -> visite_expr vis e0
  | Com.Literal _ | Com.NbCategory _ -> ()
  | Com.Var a -> visite_acces vis a
  | Com.Attribut (m_a, _)
  | Com.Size m_a
  | Com.Type (m_a, _)
  | Com.InDomain (m_a, _) ->
      visite_acces vis (Pos.unmark m_a)
  | Com.SameVariable (m_a0, m_a1) ->
      visite_acces vis (Pos.unmark m_a0);
      visite_acces vis (Pos.unmark m_a1)
  | Com.NbAnomalies | Com.NbDiscordances | Com.NbInformatives
  | Com.NbBloquantes ->
      ()

and visite_acces (vis : visite) (a : Var.t access) : unit =
  match a with
  | Com.VarAccess _ -> ()
  | Com.TabAccess (_, _, m_i) | Com.FieldAccess (_, m_i, _, _) ->
      visite_expr vis m_i

let rec visite_instr (vis : visite) (m_i : (Var.t, Error.t) m_instruction) :
    unit =
  vis.vi_instr m_i;
  let instrs = List.iter (visite_instr vis) in
  let acces = List.iter (fun m_a -> visite_acces vis (Pos.unmark m_a)) in
  match Pos.unmark m_i with
  | Com.Affectation m_f -> (
      match Pos.unmark m_f with
      | Com.SingleFormula (Com.VarDecl (_, m_e)) -> visite_expr vis m_e
      | Com.SingleFormula (Com.EventFieldRef (m_i', _, _, _)) ->
          visite_expr vis m_i'
      | Com.MultipleFormulaes (_, Com.VarDecl (_, m_e)) -> visite_expr vis m_e
      | Com.MultipleFormulaes (_, Com.EventFieldRef (m_i', _, _, _)) ->
          visite_expr vis m_i')
  | Com.IfThenElse (m_e, a, b) ->
      visite_expr vis m_e;
      instrs a;
      instrs b
  | Com.WhenDoElse (cases, m_else) ->
      List.iter
        (fun (m_e, l, _) ->
          visite_expr vis m_e;
          instrs l)
        cases;
      instrs (Pos.unmark m_else)
  | Com.ComputeDomain _ | Com.ComputeChaining _ -> ()
  | Com.ComputeVerifs (_, m_e, _) -> visite_expr vis m_e
  | Com.ComputeTarget (_, args, _) -> acces args
  | Com.VerifBlock l -> instrs l
  | Com.Print (_, args) ->
      List.iter
        (fun m_pa ->
          match Pos.unmark m_pa with
          | Com.PrintString _ -> ()
          | Com.PrintAccess (_, m_a) -> visite_acces vis (Pos.unmark m_a)
          | Com.PrintIndent m_e | Com.PrintExpr (m_e, _, _) -> visite_expr vis m_e)
        args
  | Com.Iterate (_, args, cvml, l) ->
      acces args;
      List.iter (fun (_, m_e, _) -> visite_expr vis m_e) cvml;
      instrs l
  | Com.Iterate_values (_, ivl, l) ->
      List.iter
        (fun (m_e0, m_e1, m_e2) ->
          visite_expr vis m_e0;
          visite_expr vis m_e1;
          visite_expr vis m_e2)
        ivl;
      instrs l
  | Com.Restore (args, cvml, el, vel, l) ->
      acces args;
      List.iter (fun (_, _, m_e, _) -> visite_expr vis m_e) cvml;
      List.iter (visite_expr vis) el;
      List.iter (fun (_, m_e) -> visite_expr vis m_e) vel;
      instrs l
  | Com.ArrangeEvents (sort, filter, add, l) ->
      (match sort with Some (_, _, m_e) -> visite_expr vis m_e | None -> ());
      (match filter with Some (_, m_e) -> visite_expr vis m_e | None -> ());
      (match add with Some m_e -> visite_expr vis m_e | None -> ());
      instrs l
  | Com.Switch (m_e, branches) ->
      visite_expr vis m_e;
      List.iter (fun (_, l) -> instrs l) branches
  | Com.RaiseError _ | Com.CleanErrors | Com.CleanFinalizedErrors
  | Com.ExportErrors | Com.FinalizeErrors | Com.Stop _ ->
      ()

(* Cibles appelées par un programme d'instructions (fermeture transitive). *)
let rec instrs_cibles (l : (Var.t, Error.t) m_instruction list) : string list =
  List.concat_map
    (fun m_i ->
      match Pos.unmark m_i with
      | Com.ComputeTarget (m_tn, _, _) -> [ Pos.unmark m_tn ]
      | Com.IfThenElse (_, a, b) -> instrs_cibles a @ instrs_cibles b
      | Com.WhenDoElse (cases, m_else) ->
          List.concat_map (fun (_, l, _) -> instrs_cibles l) cases
          @ instrs_cibles (Pos.unmark m_else)
      | Com.VerifBlock l -> instrs_cibles l
      | Com.Iterate (_, _, _, l)
      | Com.Iterate_values (_, _, l)
      | Com.Restore (_, _, _, _, l)
      | Com.ArrangeEvents (_, _, _, l) ->
          instrs_cibles l
      | Com.Switch (_, branches) ->
          List.concat_map (fun (_, l) -> instrs_cibles l) branches
      | _ -> [])
    l

(* Branche « si MODE_CORR + 0 = 1 alors … sinon … finsi » de la cible       *)
(* principale : rend (branche correctif, position, corps d'instructions).   *)
let rec cherche_branche_correctif (m_i : (Var.t, Error.t) m_instruction) :
    (string * Pos.t * (Var.t, Error.t) m_instruction list) option =
  match Pos.unmark m_i with
  | Com.IfThenElse (cond, alors, sinon) when mentionne_selecteur cond ->
      let branche =
        match litteral_compare cond with
        | Some f when f = 1. -> "alors"
        | Some f when f = 0. -> "sinon"
        | _ -> "indeterminee"
      in
      let corps = match branche with "sinon" -> sinon | _ -> alors in
      Some (branche, Pos.get m_i, corps)
  | Com.IfThenElse (_, alors, sinon) -> (
      match List.find_map cherche_branche_correctif alors with
      | Some r -> Some r
      | None -> List.find_map cherche_branche_correctif sinon)
  | Com.VerifBlock l -> List.find_map cherche_branche_correctif l
  | Com.WhenDoElse (cases, m_else) -> (
      match
        List.find_map
          (fun (_, l, _) -> List.find_map cherche_branche_correctif l)
          cases
      with
      | Some r -> Some r
      | None -> List.find_map cherche_branche_correctif (Pos.unmark m_else))
  | Com.Switch (_, branches) ->
      List.find_map
        (fun (_, l) -> List.find_map cherche_branche_correctif l)
        branches
  | Com.Iterate (_, _, _, l)
  | Com.Iterate_values (_, _, l)
  | Com.Restore (_, _, _, _, l)
  | Com.ArrangeEvents (_, _, _, l) ->
      List.find_map cherche_branche_correctif l
  | _ -> None

let fermeture_cibles (p : Mir.program) (depart : string list) : string list =
  let rec boucle vus frontiere =
    match frontiere with
    | [] -> List.rev vus
    | n :: r when List.mem n vus -> boucle vus r
    | n :: r ->
        let suivants =
          match StrMap.find_opt n p.program_targets with
          | Some t -> instrs_cibles t.target_prog
          | None -> []
        in
        boucle (n :: vus) (suivants @ r)
  in
  boucle [] depart

type phase_correctif = {
  pc_present : bool;
  pc_branche : string;
  pc_pos : Pos.t option;
  pc_cibles : string list;
}

let phase_absente =
  { pc_present = false; pc_branche = ""; pc_pos = None; pc_cibles = [] }

let phase_correctif (p : Mir.program) : phase_correctif =
  match StrMap.find_opt p.program_main_target p.program_targets with
  | None -> phase_absente
  | Some t -> (
      match List.find_map cherche_branche_correctif t.target_prog with
      | None -> phase_absente
      | Some (branche, pos, corps) ->
          {
            pc_present = true;
            pc_branche = branche;
            pc_pos = Some pos;
            pc_cibles = fermeture_cibles p (instrs_cibles corps);
          })

(* Liaisons de frame d'une cible : (variable, genre, position de liaison).   *)
(* Un emplacement peut porter plusieurs liaisons (iterer/restaurer frères) : *)
(* c'est la réécriture d'un même emplacement de la pile.                    *)
let liaisons_cible (t : (Var.t, Error.t) target) :
    (Var.t * string * Pos.t) list =
  let rec aux (m_i : (Var.t, Error.t) m_instruction)
      (acc : (Var.t * string * Pos.t) list) =
    let p = Pos.get m_i in
    match Pos.unmark m_i with
    | Com.Iterate (v, _, _, l) -> aux_list l ((v, "iteration", p) :: acc)
    | Com.Restore (_, cvml, _, _, l) ->
        let acc =
          List.fold_left
            (fun a (v, _, _, _) -> (v, "restauration", p) :: a)
            acc cvml
        in
        aux_list l acc
    | Com.IfThenElse (_, a, b) -> aux_list b (aux_list a acc)
    | Com.WhenDoElse (cases, m_else) ->
        let acc = List.fold_left (fun a (_, l, _) -> aux_list l a) acc cases in
        aux_list (Pos.unmark m_else) acc
    | Com.VerifBlock l -> aux_list l acc
    | Com.Iterate_values (_, _, l) | Com.ArrangeEvents (_, _, _, l) ->
        aux_list l acc
    | Com.Switch (_, branches) ->
        List.fold_left (fun a (_, l) -> aux_list l a) acc branches
    | _ -> acc
  and aux_list l acc = List.fold_left (fun a m -> aux m a) acc l in
  aux_list t.target_prog
    (List.map (fun v -> (v, "argument", Pos.get t.target_name)) t.target_args)

let json_cadre_cible (t : (Var.t, Error.t) target) : Yojson.Basic.t =
  let table : (int, (Var.t * string * Pos.t) list) Hashtbl.t =
    Hashtbl.create 7
  in
  List.iter
    (fun ((v : Var.t), genre, p) ->
      let i = Com.Var.loc_idx v in
      let l = try Hashtbl.find table i with Not_found -> [] in
      Hashtbl.replace table i ((v, genre, p) :: l))
    (liaisons_cible t);
  let emplacements =
    Hashtbl.fold (fun i l acc -> (i, l) :: acc) table []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  `Assoc
    [
      ("taille", `Int t.target_nb_refs);
      ("empile_a_entree", `Int t.target_nb_refs);
      ( "emplacements",
        `List
          (List.map
             (fun (i, l) ->
               `Assoc
                 [
                   ("idx", `Int i);
                   ( "liaisons",
                     `List
                       (List.map
                          (fun ((v : Var.t), genre, p) ->
                            `Assoc
                              [
                                ("var_id", `Int v.id);
                                ("nom", `String (nom_brut v));
                                ("genre", `String genre);
                                ("pos", json_pos p);
                              ])
                          (List.sort
                             (fun (a, _, _) (b, _, _) ->
                               compare (nom_brut a, a.Var.id) (nom_brut b, b.Var.id))
                             l)) );
                 ])
             emplacements) );
    ]

(* Opérations sur les événements déclaratifs, par cible. *)
type compte_evt = {
  mutable ce_arranger : Pos.t list;
  mutable ce_restaurer : Pos.t list;
  mutable ce_champ : Pos.t list;
  mutable ce_nb : Pos.t list;
  mutable ce_affecte : Pos.t list;
}

let compte_evt_vide () =
  {
    ce_arranger = [];
    ce_restaurer = [];
    ce_champ = [];
    ce_nb = [];
    ce_affecte = [];
  }

let compte_evt_cible (t : (Var.t, Error.t) target) : compte_evt =
  let c = compte_evt_vide () in
  let vis =
    {
      vi_instr =
        (fun m_i ->
          match Pos.unmark m_i with
          | Com.ArrangeEvents _ ->
              c.ce_arranger <- Pos.get m_i :: c.ce_arranger
          | Com.Restore (_, _, el, vel, _) when el <> [] || vel <> [] ->
              c.ce_restaurer <- Pos.get m_i :: c.ce_restaurer
          | Com.Affectation
              (Pos.Mark (Com.SingleFormula (Com.EventFieldRef _), _))
          | Com.Affectation
              (Pos.Mark (Com.MultipleFormulaes (_, Com.EventFieldRef _), _))
            ->
              c.ce_affecte <- Pos.get m_i :: c.ce_affecte
          | _ -> ());
      vi_expr =
        (fun m_e ->
          match Pos.unmark m_e with
          | Com.Var (Com.FieldAccess _) ->
              c.ce_champ <- Pos.get m_e :: c.ce_champ
          | Com.FuncCall (Pos.Mark (Com.NbEvents, _), _) ->
              c.ce_nb <- Pos.get m_e :: c.ce_nb
          | _ -> ());
    }
  in
  List.iter (visite_instr vis) t.target_prog;
  c

let json_compte_evt (c : compte_evt) : Yojson.Basic.t =
  let pos_list l = `List (List.map json_pos (List.rev l)) in
  `Assoc
    [
      ("arranger_evenements", pos_list c.ce_arranger);
      ("restaurer_evenements", pos_list c.ce_restaurer);
      ("champ_evenement", pos_list c.ce_champ);
      ("nb_evenements", pos_list c.ce_nb);
      ("affecte_champ_evenement", pos_list c.ce_affecte);
    ]

let compte_evt_non_vide (c : compte_evt) : bool =
  c.ce_arranger <> [] || c.ce_restaurer <> [] || c.ce_champ <> []
  || c.ce_nb <> [] || c.ce_affecte <> []

let cibles_evenements (p : Mir.program) : (string * compte_evt) list =
  List.filter_map
    (fun (n, t) ->
      let c = compte_evt_cible t in
      if compte_evt_non_vide c then Some (n, c) else None)
    (StrMap.bindings p.program_targets)

let json_evenements (p : Mir.program) : Yojson.Basic.t =
  `Assoc
    [
      ( "declaration",
        `Assoc
          [
            ( "source",
              `String
                "dgfip_m.ml:239-250 — déclaration `evenement` du prélude DGFiP \
                 (numero, rappel, code, montant, sens, penalite, base_tl, date, \
                 2042_rect, anc_penalite, id_evt, strate)" );
            ( "champs",
              `List
                (List.map
                   (fun (n, ef) -> json_event_field n ef)
                   (StrMap.bindings p.program_event_fields)) );
          ] );
      ( "semantique",
        `Assoc
          [
            ( "pile",
              `String
                "ctx_events : pile de tableaux d'événements \
                 (mir_interpreter.ml:69, 233, 1331)" );
            ( "champ_evenement",
              `String
                "FieldAccess lit events.(i).(j) — Numeric, ou RefVar résolu par \
                 get_var (mir_interpreter.ml:445-455)" );
            ( "affectation_champ",
              `String
                "EventFieldRef lie la case (i, j) à une variable (RefVar) \
                 (mir_interpreter.ml:855-870)" );
            ( "nb_evenements",
              `String
                "FuncCall NbEvents = longueur du tableau courant \
                 (mir_interpreter.ml:778-780)" );
            ( "arranger_evenements",
              `String
                "filtre, tri (mergeSort sur les variables R1/R2) puis empile \
                 (mir_interpreter.ml:1067-1146)" );
            ( "restaurer",
              `String
                "sauvegarde puis restaure variables et cases d'événements \
                 (mir_interpreter.ml:981-1065)" );
          ] );
      ("cibles", `Assoc (List.map (fun (n, c) -> (n, json_compte_evt c)) (cibles_evenements p)));
    ]

let json_ref_frames (p : Mir.program) : Yojson.Basic.t =
  `Assoc
    [
      ( "semantique",
        `Assoc
          [
            ( "base",
              `String "ctx_ref_org, base de la frame courante (mir_interpreter.ml:57)" );
            ( "acces",
              `String
                "ctx_ref.(ctx_ref_org + loc_idx) (mir_interpreter.ml:368-369)" );
            ( "liaison",
              `String
                "set_var_ref réécrit l'emplacement ctx_ref_org + loc_idx \
                 (mir_interpreter.ml:414-421) : la référence est une cellule \
                 mutable, reliée à chaque tour de boucle" );
            ( "empilement",
              `String
                "à l'entrée d'une cible ctx_ref_org += target_nb_refs, \
                 restauré à la sortie (mir_interpreter.ml:1294-1296)" );
            ( "taille",
              `String
                "target_nb_refs = nbRef + nb_args (mast_to_mir.ml:90-91) ; \
                 nbRef = profondeur maximale d'iterer/restaurer \
                 (mast_to_mir.ml:19-58)" );
            ( "indices_negatifs",
              `String
                "les arguments occupent [-nb_refs, -nb_refs+nb_args-1] \
                 (mast_to_mir.ml:227-241), les références d'itération et de \
                 restauration [-nbRef, -1] (mast_to_mir.ml:1036-1037, \
                 876-879) : comptées depuis la base empilée, donc négatives" );
            ( "pile_globale",
              `String
                "program_stats.nb_all_refs emplacements au total \
                 (mir_interpreter.ml:220)" );
          ] );
      ("nb_all_refs", `Int p.program_stats.nb_all_refs);
      ( "cibles",
        `Assoc
          (List.map
             (fun (n, t) -> (n, json_cadre_cible t))
             (StrMap.bindings p.program_targets
             @ StrMap.bindings p.program_functions)) );
    ]

let json_phases (p : Mir.program) (pc : phase_correctif) : Yojson.Basic.t =
  let evts = cibles_evenements p in
  let quand =
    let n = ref 0 in
    let vis =
      {
        vi_instr =
          (fun m_i ->
            match Pos.unmark m_i with
            | Com.WhenDoElse _ -> incr n
            | _ -> ());
        vi_expr = (fun _ -> ());
      }
    in
    List.iter
      (fun cible ->
        match StrMap.find_opt cible p.program_targets with
        | Some t -> List.iter (visite_instr vis) t.target_prog
        | None -> ())
      pc.pc_cibles;
    !n
  in
  `Assoc
    [
      ( "correctif",
        `Assoc
          [
            ("present", `Bool pc.pc_present);
            ("branche", `String pc.pc_branche);
            ("variable_selecteur", `String "MODE_CORR");
            ("pos", (match pc.pc_pos with None -> `Null | Some p -> json_pos p));
            ("cible_principale", `String p.program_main_target);
            ("cibles", `List (List.map (fun n -> `String n) pc.pc_cibles));
            ("quand_puis_quand", `Int quand);
            ( "source",
              `String
                "m_ext/<année>/main.m:22 — « si MODE_CORR + 0 = 1 alors \
                 calculer cible enchaineur_primitif ; calculer cible \
                 enchaineur_correctif ; sinon … finsi » ; sélecteur lu par \
                 mir_interpreter.ml:423-431" );
          ] );
      ( "rappels",
        `Assoc
          [
            ("present", `Bool (evts <> []));
            ( "cibles",
              `List (List.map (fun (n, _) -> `String n) evts) );
            ( "source",
              `String
                "m_ext/<année>/correctif.m (arranger_evenements) ; \
                 mir_interpreter.ml:1067-1146" );
          ] );
    ]

let json_var_space (sp : var_space) : Yojson.Basic.t =
  match sp with
  | None -> `Null
  | Some (m_name, idx) ->
      let name, p = Pos.to_couple m_name in
      `Assoc
        [
          ("name", `String (get_normal_var name));
          ("idx", `Int idx);
          ("pos", json_pos p);
        ]

(* Chaîne récursive accès / expressions / instructions. *)
let rec json_access (a : Var.t access) : Yojson.Basic.t =
  match a with
  | VarAccess (sp, v) ->
      `Assoc
        [
          ("type", `String "var");
          ("space", json_var_space sp);
          ("var", json_var_ref v);
        ]
  | TabAccess (sp, v, m_idx) ->
      `Assoc
        [
          ("type", `String "tab");
          ("space", json_var_space sp);
          ("var", json_var_ref v);
          ("index", json_expression m_idx);
        ]
  | FieldAccess (sp, m_idx, m_field, fidx) ->
      `Assoc
        [
          ("type", `String "field");
          ("space", json_var_space sp);
          ("index", json_expression m_idx);
          ("field", marked json_string m_field);
          ("field_idx", `Int fidx);
        ]

and json_atom (a : Var.t atom) : Yojson.Basic.t =
  match a with
  | AtomVar v -> `Assoc [ ("type", `String "var"); ("var", json_var_ref v) ]
  | AtomLiteral l ->
      `Assoc [ ("type", `String "literal"); ("value", json_literal l) ]

and json_set_value_loop (s : Var.t set_value_loop) : Yojson.Basic.t =
  match s with
  | Single m_a ->
      `Assoc [ ("type", `String "single"); ("atom", marked json_atom m_a) ]
  | Range (m_a0, m_a1) ->
      `Assoc
        [
          ("type", `String "range");
          ("from", marked json_atom m_a0);
          ("to", marked json_atom m_a1);
        ]
  | Interval (m_a0, m_a1) ->
      `Assoc
        [
          ("type", `String "interval");
          ("from", marked json_atom m_a0);
          ("to", marked json_atom m_a1);
        ]

and json_loop_variable (lv : Var.t loop_variable) : Yojson.Basic.t =
  let m_c, values = lv in
  `Assoc
    [
      ("parameter", marked (fun c -> `String (String.make 1 c)) m_c);
      ("values", `List (List.map json_set_value_loop values));
    ]

and json_loop_variables (lvs : Var.t loop_variables) : Yojson.Basic.t =
  let kind, vars =
    match lvs with
    | ValueSets l -> ("value_sets", l)
    | Ranges l -> ("ranges", l)
  in
  `Assoc
    [
      ("kind", `String kind);
      ("variables", `List (List.map json_loop_variable vars));
    ]

and json_cat_map (m : Pos.t CatVar.Map.t) : Yojson.Basic.t =
  `List
    (List.map
       (fun (cat, pos) -> `Assoc [ ("cat", json_cat cat); ("pos", json_pos pos) ])
       (CatVar.Map.bindings m))

and json_expression (m_e : Var.t m_expression) : Yojson.Basic.t =
  let e, pos = Pos.to_couple m_e in
  let core =
    match e with
    | TestInSet (positive, e0, values) ->
        let json_set_value (sv : Var.t set_value) : Yojson.Basic.t =
          match sv with
          | FloatValue m_f ->
              `Assoc [ ("type", `String "float"); ("value", marked json_float m_f) ]
          | VarValue m_a ->
              `Assoc [ ("type", `String "var"); ("access", marked json_access m_a) ]
          | IntervalValue (m_i0, m_i1) ->
              `Assoc
                [
                  ("type", `String "interval");
                  ("from", marked json_int m_i0);
                  ("to", marked json_int m_i1);
                ]
        in
        [
          ("kind", `String "dans_ensemble");
          ("positive", `Bool positive);
          ("expr", json_expression e0);
          ("values", `List (List.map json_set_value values));
        ]
    | Unop (op, e1) ->
        [
          ("kind", `String "unop");
          ("op", json_unop op);
          ("expr", json_expression e1);
        ]
    | Comparison (m_op, e1, e2) ->
        [
          ("kind", `String "comparison");
          ("op", marked json_comp_op m_op);
          ("lhs", json_expression e1);
          ("rhs", json_expression e2);
        ]
    | Binop (m_op, e1, e2) ->
        [
          ("kind", `String "binop");
          ("op", marked json_binop m_op);
          ("lhs", json_expression e1);
          ("rhs", json_expression e2);
        ]
    | Conditional (ec, et, ee) ->
        [
          ("kind", `String "si_expression");
          ("cond", json_expression ec);
          ("then", json_expression et);
          ("else", opt json_expression ee);
        ]
    | FuncCall (m_f, args) ->
        [
          ("kind", `String "funcall");
          ("func", marked json_func m_f);
          ("args", `List (List.map json_expression args));
        ]
    | FuncCallLoop (m_f, m_lvs, body) ->
        [
          ("kind", `String "funcall_boucle");
          ("func", marked json_func m_f);
          ("loop", marked json_loop_variables m_lvs);
          ("expr", json_expression body);
        ]
    | Literal l -> [ ("kind", `String "literal"); ("value", json_literal l) ]
    | Var a -> [ ("kind", `String "variable"); ("access", json_access a) ]
    | Loop (m_lvs, body) ->
        [
          ("kind", `String "pour_existence");
          ("loop", marked json_loop_variables m_lvs);
          ("expr", json_expression body);
        ]
    | NbCategory m_cats ->
        [ ("kind", `String "nb_categorie"); ("cats", json_cat_map m_cats) ]
    | Attribut (m_a, m_attr) ->
        [
          ("kind", `String "attribut");
          ("access", marked json_access m_a);
          ("attr", marked json_string m_attr);
        ]
    | Size m_a ->
        [ ("kind", `String "taille"); ("access", marked json_access m_a) ]
    | Type (m_a, m_typ) ->
        [
          ("kind", `String "type");
          ("access", marked json_access m_a);
          ("typ", marked json_value_typ m_typ);
        ]
    | SameVariable (m_a0, m_a1) ->
        [
          ("kind", `String "meme_variable");
          ("access1", marked json_access m_a0);
          ("access2", marked json_access m_a1);
        ]
    | InDomain (m_a, cvm) ->
        [
          ("kind", `String "dans_domaine");
          ("access", marked json_access m_a);
          ("cats", json_cat_map cvm);
        ]
    | NbAnomalies -> [ ("kind", `String "nb_anomalies") ]
    | NbDiscordances -> [ ("kind", `String "nb_discordances") ]
    | NbInformatives -> [ ("kind", `String "nb_informatives") ]
    | NbBloquantes -> [ ("kind", `String "nb_bloquantes") ]
  in
  `Assoc (("pos", json_pos pos) :: core)

and json_print_arg (m_p : Var.t print_arg Pos.marked) : Yojson.Basic.t =
  let p_arg, p = Pos.to_couple m_p in
  let core =
    match p_arg with
    | PrintString s -> [ ("type", `String "chaine"); ("value", `String s) ]
    | PrintAccess (info, m_a) ->
        [
          ("type", `String "acces");
          ("info", `String (match info with Name -> "nom" | Alias -> "alias"));
          ("access", marked json_access m_a);
        ]
    | PrintIndent e ->
        [ ("type", `String "indent"); ("expr", json_expression e) ]
    | PrintExpr (e, minp, maxp) ->
        [
          ("type", `String "expression");
          ("expr", json_expression e);
          ("min_precision", `Int minp);
          ("max_precision", `Int maxp);
        ]
  in
  `Assoc (("pos", json_pos p) :: core)

and json_formula_decl (fd : Var.t formula_decl) : Yojson.Basic.t =
  match fd with
  | VarDecl (m_a, m_e) ->
      `Assoc
        [
          ("type", `String "affectation");
          ("access", marked json_access m_a);
          ("expr", json_expression m_e);
        ]
  | EventFieldRef (m_idx, m_field, fidx, v) ->
      `Assoc
        [
          ("type", `String "champ_evenement");
          ("index", json_expression m_idx);
          ("field", marked json_string m_field);
          ("field_idx", `Int fidx);
          ("var", json_var_ref v);
        ]

and json_formula (f : Var.t formula) : Yojson.Basic.t =
  match f with
  | SingleFormula fd ->
      `Assoc [ ("type", `String "single"); ("decl", json_formula_decl fd) ]
  | MultipleFormulaes (m_lvs, fd) ->
      `Assoc
        [
          ("type", `String "multiple");
          ("loop", marked json_loop_variables m_lvs);
          ("decl", json_formula_decl fd);
        ]

and json_case (c : case) : Yojson.Basic.t =
  match c with
  | Default -> `Assoc [ ("case", `String "default") ]
  | Value l -> `Assoc [ ("case", `String "value"); ("value", json_literal l) ]

and json_stop_kind (s : stop_kind) : Yojson.Basic.t =
  match s with
  | SKApplication -> `String "application"
  | SKTarget -> `String "target"
  | SKFun -> `String "fonction"
  | SKId None -> `String "id"
  | SKId (Some v) -> `String ("id:" ^ v)

and json_instruction (m_i : (Var.t, Error.t) m_instruction) : Yojson.Basic.t =
  let i, pos = Pos.to_couple m_i in
  let core =
    match i with
    | Affectation m_f ->
        [ ("kind", `String "affectation"); ("formula", marked json_formula m_f) ]
    | IfThenElse (ec, tl, el) ->
        [
          ("kind", `String "si");
          ("cond", json_expression ec);
          ("then", `List (List.map json_instruction tl));
          ("else", `List (List.map json_instruction el));
        ]
    | WhenDoElse (cases, m_else) ->
        [
          ("kind", `String "quand_puis_quand");
          ("cases",
            `List
              (List.map
                 (fun (ec, dl, cpos) ->
                    `Assoc
                      [
                        ("cond", json_expression ec);
                        ("body", `List (List.map json_instruction dl));
                        ("pos", json_pos cpos);
                      ])
                 cases));
          ("else", marked (fun l -> `List (List.map json_instruction l)) m_else);
        ]
        (* F11/A1 : sémantique de la chaîne, mir_interpreter.ml:894-906. *)
        @ ann
            [
              ( "semantique",
                `Assoc
                  [
                    ("nb_branches", `Int (List.length cases));
                    ( "premier_faux",
                      `String
                        "la première condition fausse exécute sinon_faire et \
                         arrête la chaîne ; une condition indéfinie passe à la \
                         branche suivante (mir_interpreter.ml:894-906)" );
                  ] );
            ]
    | ComputeDomain (m_dom, sp) ->
        [
          ("kind", `String "calculer_domaine");
          ("domain", marked (fun l -> `List (List.map (marked json_string) l)) m_dom);
          ("space", json_var_space sp);
        ]
    | ComputeChaining (m_ch, sp) ->
        [
          ("kind", `String "calculer_enchaineur");
          ("chaining", marked json_string m_ch);
          ("space", json_var_space sp);
        ]
    | ComputeVerifs (m_dom, e, sp) ->
        [
          ("kind", `String "verifier_domaine");
          ("domain", marked (fun l -> `List (List.map (marked json_string) l)) m_dom);
          ("expr", json_expression e);
          ("space", json_var_space sp);
        ]
    | ComputeTarget (m_tn, args, sp) ->
        [
          ("kind", `String "calculer_cible");
          ("target", marked json_string m_tn);
          ("args", `List (List.map (marked json_access) args));
          ("space", json_var_space sp);
        ]
    | VerifBlock l ->
        [ ("kind", `String "bloc_verif"); ("body", `List (List.map json_instruction l)) ]
    | Print (std, args) ->
        [
          ("kind", `String "afficher");
          ("std", `String (match std with StdOut -> "out" | StdErr -> "err"));
          ("args", `List (List.map json_print_arg args));
        ]
    | Iterate (v, vars, params, body) ->
        [
          ("kind", `String "iterer");
          ("var", json_var_ref v);
          ("vars", `List (List.map (marked json_access) vars));
          ("params",
            `List
              (List.map
                 (fun (cats, e, sp) ->
                    `Assoc
                      [
                        ("cats", json_cat_map cats);
                        ("expr", json_expression e);
                        ("space", json_var_space sp);
                      ])
                 params));
          ("body", `List (List.map json_instruction body));
        ]
        (* F11/A1 : la variable d'itération est une référence liée à
           l'emplacement loc_idx de la frame de la cible
           (mir_interpreter.ml:414-421, 923-955). *)
        @ ann
            [
              ( "frame",
                `Assoc
                  [
                    ("idx", `Int (Com.Var.loc_idx v));
                    ("genre", `String "iteration");
                  ] );
            ]
    | Iterate_values (v, intervals, body) ->
        [
          ("kind", `String "iterer_valeurs");
          ("var", json_var_ref v);
          ("intervals",
            `List
              (List.map
                 (fun (e0, e1, step) ->
                    `Assoc
                      [
                        ("from", json_expression e0);
                        ("to", json_expression e1);
                        ("step", json_expression step);
                      ])
                 intervals));
          ("body", `List (List.map json_instruction body));
        ]
    | Restore (vars, params, evts, evtfs, body) ->
        [
          ("kind", `String "restaurer");
          ("vars", `List (List.map (marked json_access) vars));
          ("params",
            `List
              (List.map
                 (fun (v, cats, e, sp) ->
                    `Assoc
                      [
                        ("var", json_var_ref v);
                        ("cats", json_cat_map cats);
                        ("expr", json_expression e);
                        ("space", json_var_space sp);
                      ])
                 params));
          ("events", `List (List.map json_expression evts));
          ("event_fields",
            `List
              (List.map
                 (fun (v, e) ->
                    `Assoc [ ("var", json_var_ref v); ("expr", json_expression e) ])
                 evtfs));
          ("body", `List (List.map json_instruction body));
        ]
        (* F11/A1 : sauvegarde puis restauration des variables, des cases
           d'événements indexées (evts) et des cases filtrées par une
           expression sur tous les événements (evtfs)
           (mir_interpreter.ml:981-1065). *)
        @ ann
            [
              ( "restauration",
                `Assoc
                  [
                    ("acces", `Int (List.length vars));
                    ("parametres_variables", `Int (List.length params));
                    ("evenements_indexes", `Int (List.length evts));
                    ("evenements_filtres", `Int (List.length evtfs));
                  ] );
            ]
    | ArrangeEvents (sort, filter, add, body) ->
        [
          ("kind", `String "arranger_evenements");
          ("sort",
            (match sort with
            | None -> `Null
            | Some (v0, v1, e) ->
                `Assoc
                  [
                    ("var0", json_var_ref v0);
                    ("var1", json_var_ref v1);
                    ("expr", json_expression e);
                  ]));
          ("filter",
            (match filter with
            | None -> `Null
            | Some (v, e) ->
                `Assoc [ ("var", json_var_ref v); ("expr", json_expression e) ]));
          ("add", opt json_expression add);
          ("body", `List (List.map json_instruction body));
        ]
        (* F11/A1 : filtre, tri (mergeSort sur les variables R1/R2) puis
           empile la pile d'événements (mir_interpreter.ml:1067-1146). *)
        @ ann
            [
              ( "pile_evenements",
                `Assoc
                  [
                    ("empile", `Bool true);
                    ("tri", `Bool (sort <> None));
                    ("filtre", `Bool (filter <> None));
                    ("ajout", `Bool (add <> None));
                  ] );
            ]
    | Switch (e, branches) ->
        [
          ("kind", `String "aiguillage");
          ("expr", json_expression e);
          ("branches",
            `List
              (List.map
                 (fun (cases, body) ->
                    `Assoc
                      [
                        ("cases", `List (List.map json_case cases));
                        ("body", `List (List.map json_instruction body));
                      ])
                 branches));
        ]
    | RaiseError (m_err, m_var) ->
        [
          ("kind", `String "erreur");
          ("error", marked (fun (e : Error.t) -> `String (Pos.unmark e.name)) m_err);
          ("carrier_var", opt_marked json_string m_var);
        ]
    | CleanErrors -> [ ("kind", `String "nettoie_erreurs") ]
    | CleanFinalizedErrors -> [ ("kind", `String "nettoie_erreurs_finalisees") ]
    | ExportErrors -> [ ("kind", `String "exporte_erreurs") ]
    | FinalizeErrors -> [ ("kind", `String "finalise_erreurs") ]
    | Stop k -> [ ("kind", `String "stop"); ("what", json_stop_kind k) ]
  in
  `Assoc (("pos", json_pos pos) :: core)

(* Variable : enregistrement complet, tel que stocké dans program_dict. *)
let json_attrs (attrs : int Pos.marked StrMap.t) : Yojson.Basic.t =
  json_str_map (marked json_int) attrs

let json_table (t : Var.t Array.t option) : Yojson.Basic.t =
  match t with
  | None -> `Null
  | Some cells ->
      `Assoc
        [
          ("size", `Int (Array.length cells));
          ("cell_ids",
            `List (List.map (fun (v : Var.t) -> `Int v.id) (Array.to_list cells)));
        ]

let json_var (v : Var.t) : Yojson.Basic.t =
  let name, npos = Pos.to_couple (Var.name v) in
  let loc =
    match v.loc with
    | LocTgv (name, lt) ->
        `Assoc
          [
            ("kind", `String "tgv");
            ("name", `String name);
            ("cat_str", `String lt.loc_cat_str);
            ("cat_loc", json_cat_loc lt.loc_cat);
            ("cat_idx", `Int lt.loc_cat_idx);
            ("idx", `Int lt.loc_idx);
            ("tab_idx", `Int lt.loc_tab_idx);
          ]
    | LocTmp (name, lt) ->
        `Assoc
          [
            ("kind", `String "tmp");
            ("name", `String name);
            ("cat_idx", `Int lt.loc_cat_idx);
            ("idx", `Int lt.loc_idx);
            ("tab_idx", `Int lt.loc_tab_idx);
          ]
    | LocRef (rname, idx) ->
        `Assoc [ ("kind", `String "ref"); ("name", `String rname); ("idx", `Int idx) ]
  in
  let scope =
    match v.scope with
    | Tgv tgv ->
        [
          ("scope", `String "tgv");
          ("cat", json_cat tgv.cat);
          ("alias", opt_marked json_string tgv.alias);
          ("descr", marked json_string tgv.descr);
          ("attrs", json_attrs tgv.attrs);
          ("typ", opt json_value_typ tgv.typ);
          ("is_given_back", `Bool tgv.is_given_back);
          ("table", json_table tgv.table);
        ]
    | Temp table -> [ ("scope", `String "tmp"); ("table", json_table table) ]
    | Ref ->
        [ ("scope", `String "ref") ]
        @
        (* F11/A1 : la référence est résolue vers sa frame et son emplacement. *)
        if not !annoter_phases then []
        else
          let idx = Com.Var.loc_idx v in
          match Hashtbl.find_opt cadre_des_refs v.id with
          | None ->
              [
                ( "frame",
                  `Assoc
                    [
                      ("cible", `Null);
                      ("idx", `Int idx);
                      ("genre", `String "inconnu");
                    ] );
              ]
          | Some (cible, genre, p) ->
              [
                ( "frame",
                  `Assoc
                    [
                      ("cible", `String cible);
                      ("idx", `Int idx);
                      ("genre", `String genre);
                      ("liaison_pos", json_pos p);
                    ] );
              ]
  in
  `Assoc
    ([
       ("id", `Int v.id);
       ("name", `String name);
       ("decl_pos", json_pos npos);
       ("loc", loc);
     ]
    @ scope)

let json_error (e : Error.t) : Yojson.Basic.t =
  let name, npos = Pos.to_couple e.name in
  `Assoc
    [
      ("name", `String name);
      ("decl_pos", json_pos npos);
      ("famille", marked json_string e.famille);
      ("code_bo", marked json_string e.code_bo);
      ("sous_code", marked json_string e.sous_code);
      ("libelle", marked json_string e.libelle);
      ("is_isf", marked json_string e.is_isf);
      ("typ",
        `String
          (match e.typ with
          | Anomaly -> "anomalie"
          | Discordance -> "discordance"
          | Information -> "information"));
    ]

let json_variable_space (vs : variable_space) : Yojson.Basic.t =
  let vn, vpos = Pos.to_couple vs.vs_name in
  `Assoc
    [
      ("id", `Int vs.vs_id);
      ("decl_name", `String vn);
      ("pos", json_pos vpos);
      ("by_default", `Bool vs.vs_by_default);
      ("cats",
        `List
          (List.map
             (fun (loc, m_loc) -> `Assoc [ ("loc", marked json_cat_loc m_loc) ])
             (CatVar.LocMap.bindings vs.vs_cats)));
    ]

let json_domain (kind : string) (extra : (string * Yojson.Basic.t) list)
    (d : 'a domain) : Yojson.Basic.t =
  `Assoc
    ([
       ("kind", `String kind);
       ("id", marked json_domain_id d.dom_id);
       ("by_default", `Bool d.dom_by_default);
       ("names",
         `List
           (List.map
              (fun (id, pos) ->
                 `Assoc [ ("id", json_domain_id id); ("pos", json_pos pos) ])
              (DomainIdMap.bindings d.dom_names)));
       ("min", json_domain_id_set d.dom_min);
       ("max", json_domain_id_set d.dom_max);
       ("rov", `List (List.map (fun i -> `Int i) (IntSet.elements d.dom_rov)));
       ("used", opt_marked json_int d.dom_used);
     ]
    @ extra)

let json_rule_domain (d : rule_domain) : Yojson.Basic.t =
  json_domain "regle" [ ("computable", `Bool d.dom_data.rdom_computable) ] d

let json_verif_domain (d : verif_domain) : Yojson.Basic.t =
  json_domain "verif"
    [
      ("verifiable", `Bool d.dom_data.vdom_verifiable);
      ("auth",
        `List
          (List.map
             (fun (cat, pos) ->
                `Assoc [ ("cat", json_cat cat); ("pos", json_pos pos) ])
             (CatVar.Map.bindings d.dom_data.vdom_auth)));
    ]
    d

let json_var_category (data : CatVar.data) : Yojson.Basic.t =
  `Assoc
    [
      ("id_str", `String data.id_str);
      ("id_int", `Int data.id_int);
      ("cat", json_cat data.id);
      ("pos", json_pos data.pos);
      ("attributs", json_str_map json_pos data.attributs);
    ]

let json_target (t : (Var.t, Error.t) target) : Yojson.Basic.t =
  `Assoc
    ([
      ("name", `String (Pos.unmark t.target_name));
      ("pos", m_pos t.target_name);
      ("file", opt json_string t.target_file);
      ("apps", json_str_map (marked json_string) t.target_apps);
      ("args", `List (List.map json_var_ref t.target_args));
      ("result", opt json_var_ref t.target_result);
      ("tmp_vars", json_str_map (fun (v : Var.t) -> `Int v.id) t.target_tmp_vars);
      ("nb_tmps", `Int t.target_nb_tmps);
      ("sz_tmps", `Int t.target_sz_tmps);
      ("nb_refs", `Int t.target_nb_refs);
      ("is_function", `Bool (target_is_function t));
      ("prog", `List (List.map json_instruction t.target_prog));
    ]
    @
    (* F11/A1 : résolution de la frame et des opérations d'événements de la
       cible (format 2 seulement). *)
    if not !annoter_phases then []
    else
      [
        ("ref_frame", json_cadre_cible t);
        ("evenements", json_compte_evt (compte_evt_cible t));
      ])

let json_stats (s : Mir.stats) : Yojson.Basic.t =
  `Assoc
    [
      ("nb_computed", `Int s.nb_computed);
      ("nb_base", `Int s.nb_base);
      ("nb_input", `Int s.nb_input);
      ("nb_vars", `Int s.nb_vars);
      ("nb_all_tmps", `Int s.nb_all_tmps);
      ("nb_all_refs", `Int s.nb_all_refs);
      ("sz_computed", `Int s.sz_computed);
      ("sz_base", `Int s.sz_base);
      ("sz_input", `Int s.sz_input);
      ("sz_vars", `Int s.sz_vars);
      ("sz_all_tmps", `Int s.sz_all_tmps);
      ("nb_all_tables", `Int s.nb_all_tables);
      ("sz_all_tables", `Int s.sz_all_tables);
      ("max_nb_args", `Int s.max_nb_args);
      ("table_map", json_int_map json_var_ref s.table_map);
    ]

(* Tri-valeur de précision : lecture des refs Config pour la provenance. *)
let json_value_sort (v : Config.value_sort) : string =
  match v with
  | Config.RegularFloat -> "double"
  | Config.Interval -> "interval"
  | Config.Rational -> "mpq"
  | Config.BigInt _ -> "bigint"
  | Config.MPFR _ -> "mpfr"

let json_round_ops (r : Config.round_ops) : string =
  match r with
  | Config.RODefault -> "default"
  | Config.ROMulti -> "multi"
  | Config.ROMainframe sz -> "mainframe" ^ string_of_int sz

let program_to_json (p : Mir.program) : Yojson.Basic.t =
  (* F11/A1 : le document passe en version 2 (sections phases/ref_frames/
     evenements) si et seulement si le programme porte la phase correctif ;
     les programmes primitifs restent en version 1, octet pour octet. *)
  let pc = phase_correctif p in
  annoter_phases := pc.pc_present;
  Hashtbl.reset cadre_des_refs;
  if pc.pc_present then
    List.iter
      (fun (cible, t) ->
        List.iter
          (fun ((v : Var.t), genre, pos) ->
            if Com.Var.is_ref v then
              Hashtbl.replace cadre_des_refs v.id (cible, genre, pos))
          (liaisons_cible t))
      (StrMap.bindings p.program_targets @ StrMap.bindings p.program_functions);
  `Assoc
    ([
      ("format", `String "mlang-mir-json");
      ("format_version", `Int (if pc.pc_present then 2 else 1));
      ("provenance",
        `Assoc
          [
            ("mlang_version",
              (match Build_info.V1.version () with
              | None -> `Null
              | Some v -> `String (Build_info.V1.Version.to_string v)));
            ("income_year", `Int !Config.income_year);
            ("value_sort", `String (json_value_sort !Config.value_sort));
            ("round_ops", `String (json_round_ops !Config.round_ops));
            ("comparison_error_margin", `Float !Config.comparison_error_margin);
            ("requested_main_target", `String !Config.mpp_function);
            ("applications",
              `List (List.map (fun a -> `String a) !Config.application_names));
            ("source_files",
              `List
                (List.map (fun f -> `String f) (Config.get_files !Config.source_files)));
          ]);
      ("program",
        `Assoc
          [
            ("safe_prefix", `String p.program_safe_prefix);
            ("main_target", `String p.program_main_target);
            ("applications", json_str_map json_pos p.program_applications);
            ("var_categories",
              `List
                (List.map
                   (fun (_cat, data) -> json_var_category data)
                   (CatVar.Map.bindings p.program_var_categories)));
            ("rule_domains",
              `List
                (List.map
                   json_rule_domain
                   (List.map snd (DomainIdMap.bindings p.program_rule_domains))));
            ("verif_domains",
              `List
                (List.map
                   json_verif_domain
                   (List.map snd (DomainIdMap.bindings p.program_verif_domains))));
            ("vars_by_id", json_int_map json_var p.program_dict);
            ("vars", json_str_map (fun (v : Var.t) -> `Int v.id) p.program_vars);
            ("alias", json_str_map (fun (v : Var.t) -> `Int v.id) p.program_alias);
            ("var_spaces", json_str_map (fun i -> `Int i) p.program_var_spaces);
            ("var_spaces_by_id",
              json_int_map json_variable_space p.program_var_spaces_idx);
            ("default_var_space", `Int p.program_var_space_def.vs_id);
            ("event_fields",
              `List
                (List.map
                   (fun (n, ef) -> json_event_field n ef)
                   (StrMap.bindings p.program_event_fields)));
            ("event_field_idxs",
              json_int_map json_string p.program_event_field_idxs);
            ("errors", json_str_map json_error p.program_errors);
            ("rules", json_int_map json_string p.program_rules);
            ("verifs", json_int_map json_string p.program_verifs);
            ("chainings", json_str_map json_string p.program_chainings);
            ("functions", json_str_map json_target p.program_functions);
            ("targets", json_str_map json_target p.program_targets);
            ("stats", json_stats p.program_stats);
          ]);
    ]
    @
    if not pc.pc_present then []
    else
      [
        ("phases", json_phases p pc);
        ("ref_frames", json_ref_frames p);
        ("evenements", json_evenements p);
      ])

let write_program (path : string) (p : Mir.program) : unit =
  let json = program_to_json p in
  Yojson.Basic.to_file path json
