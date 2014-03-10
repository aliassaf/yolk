(** Commands used in Coq to interact with the Yolk plugin **)

VERNAC COMMAND EXTEND YolkLibrary
| [ "Yolk" "Library" global(reference) ] ->
  [ Libraries.export_library reference ]
END

VERNAC COMMAND EXTEND YolkAll
| [ "Yolk" "All" ] ->
  [ Libraries.export_all () ]
END

