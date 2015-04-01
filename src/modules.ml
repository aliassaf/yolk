(** Export of Coq modules **)

module B = Backend
module D = Declarations

(** Constant definitions have a type and a body.
    - The type can be non-polymorphic (normal type) or
      a polymorphic arity (universe polymorphism).
    - The body can be empty (an axiom), a normal definition, or
      an opaque definition (a theorem). **)

let polymorphic_arity env a =
  let poly_param b =
    match b with
    | None -> B.variant "None" []
    | Some(u) -> B.variant "Some" [Sorts.universe env u]
  in
  B.Record [
    "param_levels", B.List (List.map poly_param a.D.poly_param_levels);
    "level", Sorts.universe env a.D.poly_level;
    ]

let constant_type env ct =
  match ct with
  | D.NonPolymorphicType a ->
    B.variant "NonPolymorphicType" [Terms.constr env a]
  | D.PolymorphicArity (ctx, pa) ->
    B.variant "PolymorphicArity" [
      Terms.rel_context env ctx;
      polymorphic_arity env pa;
      ]

let constant_def env cd =
  match cd with
  | D.Undef _ ->
    B.variant "Undef" []
  | D.Def cs ->
    let m = Declarations.force cs in
    B.variant "Def" [Terms.constr env m]
  | D.OpaqueDef lc ->
    let m = Declarations.force_opaque lc in
    B.variant "OpaqueDef" [Terms.constr env m]

let constant_body env cb =
  (* There should be no section hypotheses at this stage. *)
  assert (List.length cb.D.const_hyps = 0);
  assert (Univ.is_empty_constraint cb.D.const_constraints);
  B.Record [
    "type", constant_type env cb.D.const_type;
    "body", constant_def env cb.D.const_body;
    ]

(** An inductive definition is organised into:
    - [mutual_inductive_body] : a block of (co)inductive type definitions,
      containing a context of common parameter and list of [inductive_body]
    - [inductive_body] : a single inductive type definition,
      containing a name, an arity, and a list of constructor names and types **)

let monomorphic_inductive_arity env mia =
  B.Record [
    "user_arity", Terms.constr env mia.D.mind_user_arity;
    "sort", Terms.sorts env mia.D.mind_sort;
    ]

let inductive_arity env ia =
  match ia with
  | D.Monomorphic mia ->
    B.variant "Monomorphic" [monomorphic_inductive_arity env mia]
  | D.Polymorphic pa ->
    B.variant "Polymorphic" [polymorphic_arity env pa]

let one_inductive_body env ib =
  B.Record [
    "typename", B.String (Names.string_of_id ib.D.mind_typename);
    "arity_ctxt", Terms.rel_context env ib.D.mind_arity_ctxt;
    "arity", inductive_arity env ib.D.mind_arity;
    "consnames", B.List (List.map (fun id -> B.String (Names.string_of_id id)) (Array.to_list ib.D.mind_consnames));
    "user_lc", B.List (List.map (Terms.constr env) (Array.to_list ib.D.mind_user_lc));
    ]

let mutual_inductive_body env mib =
  (* There should be no section hypotheses at this stage. *)
  assert (List.length mib.D.mind_hyps = 0);
  assert (Univ.is_empty_constraint mib.D.mind_constraints);
  B.Record [
    "finite", B.Bool mib.D.mind_finite;
    "record", B.Bool mib.D.mind_record;
    "ntypes", B.Int mib.D.mind_ntypes;
    "nparams", B.Int mib.D.mind_nparams;
    "nparams_rec", B.Int mib.D.mind_nparams_rec;
    "params_ctxt", Terms.rel_context env mib.D.mind_params_ctxt;
    "packets", B.List (List.map (one_inductive_body env) (Array.to_list mib.D.mind_packets));
    ]

(** Modules are organised into:
    - [module_body] (mb): a wrapper around a struct expression
    - [struct_expr_body] (seb): a struct expression, e.g. functor,
      application, ...
    - [structure_body] (sb): a concrete struct, i.e. a list of fields
    - [structure_field_body] (sfb): a single field declaration, e.g.
      definition, inductive, ... **)

let rec module_body env mb =
  assert (Univ.is_empty_constraint mb.D.mod_constraints);
  B.Record [
    "mp", B.String (Names.string_of_mp mb.D.mod_mp);
    "expr",
      begin match mb.D.mod_expr with
      | Some(seb) -> struct_expr_body env seb
      | None -> failwith "Empty module body"
      end;
    ]

and struct_expr_body env seb =
  match seb with
  | D.SEBident _ -> Error.not_supported "SEBident"
  | D.SEBfunctor _ -> Error.not_supported "SEBfunctor"
  | D.SEBapply _ -> Error.not_supported "SEBapply"
  | D.SEBstruct sb ->
    B.variant "SEBstruct" [
      structure_body env sb
    ]
  | D.SEBwith(_) -> Error.not_supported "SEBwith"

and structure_body env sb =
  let structure_body_entry (lbl, body) =
    B.Tuple [B.String (Names.string_of_label lbl); structure_field_body env body]
  in
  B.List (List.map structure_body_entry sb)

and structure_field_body env sfb =
  match sfb with
  | D.SFBconst(cb) ->
    B.variant "SFBconst" [constant_body env cb]
  | D.SFBmind(mib) ->
    B.variant "SFBmind" [mutual_inductive_body env mib]
  | D.SFBmodule(mb) ->
    B.variant "SFBmodule" [module_body env mb]
  | D.SFBmodtype _ -> Error.not_supported "SFBmodtype"
