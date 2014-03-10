(** Export of Coq modules *)

open Declarations

(**
  Constant definitions have a type and a body.
  - The type can be non-polymorphic (normal type) or
    a polymorphic arity (universe polymorphism).
  - The body can be empty (an axiom), a normal definition, or
    an opaque definition (a theorem).
  *)

let export_non_polymorphic_type out env =
  Format.fprintf out "NonPolymorphicType"

let export_polymorphic_arity out env =
  Format.fprintf out "PolymorphicArity"

let export_undef out env inline =
  (* For now assume inline is None. *)
  assert (inline = None);
  Format.fprintf out "Axiom"

let export_def out env constr_substituted =
  Format.fprintf out "@[<2>Def(@,";
  let constr = Declarations.force constr_substituted in
  Terms.export_constr out env constr;
  Format.fprintf out "@,)@]"

let export_opaque_def out env lazy_constr =
  Format.fprintf out "@[<2>Opaque(@,";
  let constr = Declarations.force_opaque lazy_constr in
  Terms.export_constr out env constr;
  Format.fprintf out "@,)@]"

let export_constant_body out env cb =
  Format.fprintf out "@[<2>Constant(@,";
  (* There should be no section hypotheses at this stage. *)
  assert (List.length cb.const_hyps = 0);
  begin match cb.const_type with
  | NonPolymorphicType(_) -> export_non_polymorphic_type out env
  | PolymorphicArity(_) -> export_polymorphic_arity out env
  end;
  Format.fprintf out ",@ ";
  begin match cb.const_body with
  | Undef(inline) -> export_undef out env inline
  | Def(constr_substituted) -> export_def out env constr_substituted
  | OpaqueDef(lazy_constr) -> export_opaque_def out env lazy_constr
  end;
  Format.fprintf out "@,)@]"


let export_mutual_inductive_body out env mib =
  Format.fprintf out "@[<2>Inductive(@,";
  Format.fprintf out "@,)@]"

(**
  Modules are organised into:
  - module_body (mb):
    a wrapper around a struct expression
  - struct_expr_body (seb):
    a struct expression, e.g. functors, applications, ...
  - structure_body (sb):
    a concrete struct, i.e. a list of fields
  - structure_field_body (sfb):
    a single field declaration, e.g. definitions, inductives, ...
  **)

let rec export_module_body out env mb =
  Format.fprintf out "@[<2>Module(@,";
  begin match mb.mod_expr with
  | Some(seb) -> export_struct_expr_body out env seb
  | None -> failwith "Empty module body"
  end;
  Format.fprintf out "@,)@]"

and export_struct_expr_body out env seb =
  match seb with
  | SEBident(_) -> failwith "SEBident not supported"
  | SEBfunctor(_) -> failwith "SEBfunctor not supported"
  | SEBapply(_) -> failwith "SEBapply not supported"
  | SEBstruct(sb) -> export_structure_body out env sb
  | SEBwith(_) -> failwith "SEBwith not supported"

and export_structure_body out env sb =
  Format.fprintf out "@[<hv2>Struct([@,";
  List.iter (export_structure_field_body out env) sb;
  Format.fprintf out "])@]"

and export_structure_field_body out env (label, sfb) =
  Format.fprintf out "@[<2>(%s,@ " (Names.string_of_label label);
  begin match sfb with
  | SFBconst(cb) -> export_constant_body out env cb
  | SFBmind(mib) -> export_mutual_inductive_body out env mib
  | SFBmodule(mb) -> export_module_body out env mb
  | SFBmodtype(_) -> failwith "SFBmodtype not supported"
  end;
  Format.fprintf out "@,)@];@ "

