(** Main export functions **)

open Declarations

let export_struct_expr_body formatter env struct_expr_body = ()

(** Export the library referred to by [qualid].
    A libray is a module that corresponds to a file on disk. **)
let export_qualified_library qualid =
  (* Locate the module associated to the library. *)
  let module_path =
    try Nametab.locate_module qualid with
    | Not_found -> Error.unknown_module qualid
  in
  (* Get the structure of the module. *)
  let module_body = Global.lookup_module module_path in
  let struct_expr_body =
    match module_body.mod_expr with
    | None -> failwith "empty library body"
    | Some(struct_expr_body) -> struct_expr_body
  in
  (* Export the structure to a file of the same name.  *)
  let env = Global.env () in
  let out_channel = open_out (Names.string_of_mp module_path) in
  let formatter = Format.formatter_of_out_channel out_channel in
  try
    export_struct_expr_body formatter env struct_expr_body;
    Format.pp_print_flush formatter ();
    close_out out_channel
  with e ->
    Format.pp_print_flush formatter ();
    close_out out_channel;
    raise e

let export_library reference =
  let _, qualid = Libnames.qualid_of_reference reference in
  export_qualified_library qualid

(** Export all loaded libraries. **)
let export_all () =
  let dirpaths = Library.loaded_libraries () in
  let qualids = List.map Libnames.qualid_of_dirpath dirpaths in
  List.iter export_qualified_library qualids

