(** Main export functions **)

let export_library ident =
  let qualid = Libnames.qualid_of_ident ident in
  let dirpath =
    try Nametab.full_name_module qualid
    with Not_found -> Error.unknown_module qualid
  in
  ()

let export_all () = ()

