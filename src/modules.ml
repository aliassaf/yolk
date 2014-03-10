(** Export of Coq modules *)

open Declarations

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
  match mb.mod_expr with
  | Some(seb) -> export_struct_expr_body out env seb
  | None -> failwith "Empty module body"

and export_struct_expr_body out env seb =
  match seb with
  | SEBident(_) -> failwith "SEBident not supported"
  | SEBfunctor(_) -> failwith "SEBfunctor not supported"
  | SEBapply(_) -> failwith "SEBapply not supported"
  | SEBstruct(sb) -> export_structure_body out env sb
  | SEBwith(_) -> failwith "SEBwith not supported"

and export_structure_body out env sb =
  List.iter (export_structure_field_body out env) sb

and export_structure_field_body out env (label, sfb) =
  ()

