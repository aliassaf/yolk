(** Pretty-printed warning and error messages **)

open Pp

let error message =
  Util.errorlabstrm "Yolk" message

let unknown_module qualid =
  error (str "Module" ++ spc () ++ Libnames.pr_qualid qualid ++ spc () ++ str "not found.")

