(** Test the features of the Yolk plugin. **)

Require Yolk.

Print ML Modules.

Print Libraries.

Cd "test".

Yolk Library Peano.

Yolk Library Coq.Init.Peano.

Module M.

End M.

(* M is not a library. *)

(*Yolk Library M.*)

Module Peano1 := Peano.

(* Peano1 is not a library. *)

(*Yolk Library Peano1.*)

Yolk All.

