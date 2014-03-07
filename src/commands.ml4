(** Commands used in Coq to interact with the Yolk plugin **)

VERNAC COMMAND EXTEND YolkLibrary
| [ "Yolk" "Library" global(reference) ] -> [ Export.export_library reference ]
END

VERNAC COMMAND EXTEND YolkAll
| [ "Yolk" "All" ] -> [ Export.export_all () ]
END

