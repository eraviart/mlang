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
    | Ref -> [ ("scope", `String "ref") ]
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
    [
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
  `Assoc
    [
      ("format", `String "mlang-mir-json");
      ("format_version", `Int 1);
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

let write_program (path : string) (p : Mir.program) : unit =
  let json = program_to_json p in
  Yojson.Basic.to_file path json
