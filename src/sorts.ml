(** Export Coq sorts **)

module B = Backend

(** An algebraic universe [u] is a universe expression that can be
    the successor, the max, or a an abstract universe variable.
    It can even correspond to Prop for some values of [u].
    The structure of an algebraic universe cannot be inspected
    directly because it has an abstract type. **)
let universe env u =
  B.String (Pp.string_of_ppcmds (Univ.pr_uni u))

let prop env =
  universe env (Univ.type0m_univ)

let type0 env =
  universe env (Univ.type0_univ)
