(** Export of Coq modules **)

open Declarations

(** Constant definitions have a type and a body.
    - The type can be non-polymorphic (normal type) or
      a polymorphic arity (universe polymorphism).
    - The body can be empty (an axiom), a normal definition, or
      an opaque definition (a theorem). **)


let export_polymorphic_arity env out a =
  let export_poly_param out b =
    match b with
    | None ->
      Format.fprintf out "None"
    | Some(u) ->
      Output.open_box out "Some";
      Sorts.export_universe env out u;
      Output.close_box out ();
  in
  Output.open_box out "";
  Output.open_list_box out "";
  Output.sep_list_box out export_poly_param a.poly_param_levels;
  Output.close_list_box out ();
  Output.sep_box out ();
  Sorts.export_universe env out a.poly_level;
  Output.close_box out ()

let export_monomorphic_constant_type env out a =
  Output.open_box out "MonomorphicConstantType";
  Terms.export_constr env out a;
  Output.close_box out ()

let export_polymorphic_constant_arity env out ctx s =
  Output.open_box out "PolymorphicConstantArity";
  Terms.export_rel_context env out ctx;
  export_polymorphic_arity env out s;
  Output.close_box out ()

let export_undef env out inline =
  (* For now assume inline is None. *)
  assert (inline = None);
  Format.fprintf out "Axiom"

let export_def env out constr_substituted =
  Output.open_box out "Definition";
  let constr = Declarations.force constr_substituted in
  Terms.export_constr env out constr;
  Output.close_box out ()

let export_opaque_def env out lazy_constr =
  Output.open_box out "Opaque";
  let constr = Declarations.force_opaque lazy_constr in
  Terms.export_constr env out constr;
  Output.close_box out ()

let export_constant_body env out cb =
  (* There should be no section hypotheses at this stage. *)
  assert (List.length cb.const_hyps = 0);
  Output.open_box out "Constant";
  begin match cb.const_type with
  | NonPolymorphicType(a) -> export_monomorphic_constant_type env out a
  | PolymorphicArity(ctx, s) -> export_polymorphic_constant_arity env out ctx s
  end;
  Output.sep_box out ();
  begin match cb.const_body with
  | Undef(inline) -> export_undef env out inline
  | Def(constr_substituted) -> export_def env out constr_substituted
  | OpaqueDef(lazy_constr) -> export_opaque_def env out lazy_constr
  end;
  Output.close_box out ()

(** An inductive definition is organised into:
    - [mutual_inductive_body] : a block of (co)inductive type definitions,
      containing a context of common parameter and list of [inductive_body]
    - [inductive_body] : a single inductive type definition,
      containing a name, an arity, and a list of constructor names and types **)

let export_monomorphic_inductive_arity env out a ctx =
  (* User arity should be redundant. *)
  let arity = Term.it_mkProd_or_LetIn (Term.mkSort a.mind_sort) ctx in
  assert (arity = a.mind_user_arity);
  Output.open_box out "MonomorphicInductiveArity";
  Terms.export_sort env out a.mind_sort;
  Output.close_box out ()

let export_polymorphic_inductive_arity env out a =
  Output.open_box out "PolymorphicInductiveArity";
  export_polymorphic_arity env out a;
  Output.close_box out ()

let export_constructor env out (c, a) =
  Output.open_box out "";
  Format.fprintf out "%s" (Names.string_of_id c);
  Output.sep_box out ();
  Terms.export_constr env out a;
  Output.close_box out ()

let export_inductive_body env out ib =
  let constructors =
    List.combine
      (Array.to_list ib.mind_consnames)
      (Array.to_list ib.mind_user_lc)
  in
  Output.open_box out "";
  Format.fprintf out "%s" (Names.string_of_id ib.mind_typename);
  Output.sep_box out ();
  Terms.export_rel_context env out ib.mind_arity_ctxt;
  Output.sep_box out ();
  begin match ib.mind_arity with
  | Monomorphic(a) -> export_monomorphic_inductive_arity env out a ib.mind_arity_ctxt
  | Polymorphic(a) -> export_polymorphic_inductive_arity env out a
  end;
  Output.sep_box out ();
  Output.open_list_box out "";
  Output.sep_list_box out (export_constructor env) constructors;
  Output.close_list_box out ();
  Output.close_box out ()

let export_mutual_inductive_body env out mib =
  (* There should be no section hypotheses at this stage. *)
  assert (List.length mib.mind_hyps = 0);
  Output.open_box out "Inductive";
  Format.fprintf out "%B" mib.mind_finite;
  Output.sep_box out ();
  Format.fprintf out "%d" mib.mind_nparams;
  Output.sep_box out ();
  Format.fprintf out "%d" mib.mind_nparams_rec;
  Output.sep_box out ();
  Terms.export_rel_context env out mib.mind_params_ctxt;
  Output.sep_box out ();
  Output.open_list_box out "";
  Output.sep_list_box out (export_inductive_body env) (Array.to_list mib.mind_packets);
  Output.close_list_box out ();
  Output.close_box out ()

(** Modules are organised into:
    - [module_body] (mb): a wrapper around a struct expression
    - [struct_expr_body] (seb): a struct expression, e.g. functor,
      application, ...
    - [structure_body] (sb): a concrete struct, i.e. a list of fields
    - [structure_field_body] (sfb): a single field declaration, e.g.
      definition, inductive, ... **)

let rec export_module_body env out mb =
  Output.open_box out "Module";
  begin match mb.mod_expr with
  | Some(seb) -> export_struct_expr_body env out seb
  | None -> failwith "Empty module body"
  end;
  Output.close_box out ()

and export_struct_expr_body env out seb =
  match seb with
  | SEBident(_) -> failwith "SEBident not supported"
  | SEBfunctor(_) -> failwith "SEBfunctor not supported"
  | SEBapply(_) -> failwith "SEBapply not supported"
  | SEBstruct(sb) -> export_structure_body env out sb
  | SEBwith(_) -> failwith "SEBwith not supported"

and export_structure_body env out sb =
  Output.open_list_box out "Struct";
  Output.sep_list_box out (export_structure_field_body env) sb;
  Output.close_list_box out ();

and export_structure_field_body env out (label, sfb) =
  Output.open_box out "";
  Format.fprintf out "%s" (Names.string_of_label label);
  Output.sep_box out ();
  begin match sfb with
  | SFBconst(cb) -> export_constant_body env out cb
  | SFBmind(mib) -> export_mutual_inductive_body env out mib
  | SFBmodule(mb) -> export_module_body env out mb
  | SFBmodtype(_) -> failwith "SFBmodtype not supported"
  end;
  Output.close_box out ()

