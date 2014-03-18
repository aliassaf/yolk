(** Pretty-printed warning and error messages **)

open Pp

let error message =
  Util.errorlabstrm "Yolk" message

let not_supported feature =
  error (str feature ++ str " not supported")

