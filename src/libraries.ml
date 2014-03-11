(** Export of Coq libraries *)

open Pp

(** Export the library referred to by [qualid].
    A libray is a module that corresponds to a file on disk. **)
let export_qualified_library qualid =
  Pp.msgnl (str "Exporting " ++ Libnames.pr_qualid qualid);
  let module_path = Nametab.locate_module qualid in
  let module_body = Global.lookup_module module_path in
  let filename = Names.string_of_mp module_path ^ ".ylk" in
  let out_channel = open_out filename in
  let formatter = Format.formatter_of_out_channel out_channel in
  try
    Modules.export_module_body (Global.env ()) formatter module_body;
    Format.pp_print_flush formatter ();
    close_out out_channel
  with e ->
    Format.pp_print_flush formatter ();
    close_out out_channel;
    raise e

(** Export the library referred to by [reference]. *)
let export_library reference =
  let loc, qualid = Libnames.qualid_of_reference reference in
  Library.require_library [loc, qualid] None;
  export_qualified_library qualid

(** Export all loaded libraries. **)
let export_all () =
  let dirpaths = Library.loaded_libraries () in
  let qualids = List.map Libnames.qualid_of_dirpath dirpaths in
  List.iter export_qualified_library qualids

