(** Export of Coq sorts **)

let export_prop env out =
  Format.fprintf out "Prop"

let export_type0 env out =
  Format.fprintf out "Type(0)"

(** An algebraic universe [u] is a universe expression that can be
    the successor, the max, or a an abstract universe variable.
    It can even  correspond to Prop for some values of [u].
    The structure of an algebraic universe cannot be inspected
    directly because it has an abstract type. **)
let export_univ env out u =
  if Univ.is_type0m_univ u then export_prop env out
  else if Univ.is_type0_univ u then export_type0 env out
  else Format.fprintf out "Univ(%s)" (Pp.string_of_ppcmds (Univ.pr_uni u))

