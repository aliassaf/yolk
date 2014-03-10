(** Export of Coq modules *)

open Declarations

let export_constant_body out env cb =
  Format.fprintf out "@[<2>Constant(@,";
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

