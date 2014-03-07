(** Commands used in Coq to interact with the Yolk plugin **)

VERNAC COMMAND EXTEND YolkLibrary
| [ "Yolk" "Library" ident(m) ] -> [ Export.export_library m ]
END

VERNAC COMMAND EXTEND YolkAll
| [ "Yolk" "All" ] -> [ Export.export_all () ]
END

