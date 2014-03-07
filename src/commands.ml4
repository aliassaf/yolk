(** Commands used in Coq to interact with the plugin **)

let export_library m = ()

let export_all () = ()

VERNAC COMMAND EXTEND YolkLibrary
| [ "Yolk" "Library" ident(m) ] -> [ export_library m ]
END

VERNAC COMMAND EXTEND YolkAll
| [ "Yolk" "All" ] -> [ export_all () ]
END

