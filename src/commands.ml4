(** Commands used in Coq to interact with the Yolk plugin **)

VERNAC COMMAND EXTEND YolkModule
| [ "Yolk" "Module" global(r) ] -> [
    let (_, qid) = Libnames.qualid_of_reference r in
    let mp = Nametab.locate_module qid in
    let mb = Global.lookup_module mp in
    let data = Modules.module_body (Global.env ()) mb in
    Backend.Json.write stdout data
  ]
END
