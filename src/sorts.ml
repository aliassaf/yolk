(** Export of Coq sorts **)

let export_prop env out =
  Format.fprintf out "Prop"

let export_type0 env out =
  Format.fprintf out "Type(0)"

(** An algebraic universe [u] can correspond to Prop for some
    values of [u]. *)
let export_univ env out u =
  Format.fprintf out "Univ"

