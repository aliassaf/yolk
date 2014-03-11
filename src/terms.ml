(** Export of Coq terms **)

open Term

(** The sorts of the pCIC are either Prop or Type i.
    The actual implementation is a bit different:
    - [Prop(Null)] corresponds to Prop
    - [Prop(Pos)] actually corresponds to Type 0 (probably a remnant
      of the impredicative Set in the original CIC)
    - [Type(u)] is an algebraic universe and can even correspond
      to Prop for some values of [u]. **)
let export_sort env out s =
  Output.open_box out "Sort";
  begin match s with
  | Prop(Null) -> Sorts.export_prop env out
  | Prop(Pos) -> Sorts.export_type0 env out
  | Type(u) -> Sorts.export_univ env out u
  end;
  Output.close_box out ()

(** A constant [c] declared in [env] **)
let export_constant env out c =
  Format.fprintf out "Const(%s)" (Names.string_of_con c)

(** The [i]-th inductive type of the block of mutual inductive types
    [mind] declared in [env]  **)
let export_inductive env out (mind, i) =
  Format.fprintf out "Ind(%s, %d)" (Names.string_of_mind mind) i

(** The [j]-th constructor of the [i]-th inductive type of the block
    of mutual inductive types [mind] declared in [env]  **)
let export_constructor env out ((mind, i), j) =
  Format.fprintf out "Construct(%s, %d, %d)" (Names.string_of_mind mind) i j

(** The name [x] of a local variable (used in lambdas, products,
    and letins) to be pushed in [env] **)
let export_name env out x =
  match x with
  | Names.Name(id) -> Format.fprintf out "%s" (Names.string_of_id id)
  | Names.Anonymous -> Format.fprintf out "_"

(** A local variable represented in de Bruijn index pointing to
    the [i]-th entry in [env] **)
let export_rel env out i =
  Format.fprintf out "Rel(%d)" i

(** The terms of the pCIC **)
let rec export_constr env out m =
  match Term.kind_of_term m with
  | Rel(i) -> export_rel env out i
  | Var(_) -> Error.not_supported "Var"
  | Meta(_) -> Error.not_supported "Meta"
  | Evar(_) -> Error.not_supported "Evar"
  | Sort(s) -> export_sort env out s
  | Cast(m, cast_kind, a) -> export_cast env out cast_kind m a
  | Prod(x, a, b) -> export_prod env out x a b
  | Lambda(x, a, m) -> export_lam env out x a m
  | LetIn(x, n, a, m) -> export_let_in env out x n a m
  | App(m, args) -> export_app env out m args
  | Const(c) -> export_constant env out c
  | Ind(ind) -> export_inductive env out ind
  | Construct(c) -> export_constructor env out c
  | Case(info, p, m, b) -> export_case env out info p m b
  | Fix(f) -> export_fixpoint env out f
  | CoFix(f) -> Error.not_supported "CoFix"

(** [forall x : a, b] **)
and export_prod env out x a b =
  Output.open_box out "Prod";
  export_name env out x;
  Output.sep_box out ();
  export_constr env out a;
  Output.sep_box out ();
  export_constr (Environ.push_rel (x, None, a) env) out b;
  Output.close_box out ()

(** [fun x : a => m] **)
and export_lam env out x a m =
  Output.open_box out "Lam";
  export_name env out x;
  Output.sep_box out ();
  export_constr env out a;
  Output.sep_box out ();
  export_constr (Environ.push_rel (x, None, a) env) out m;
  Output.close_box out ()
    
(** [m args] **)
and export_app env out m args =
  Output.open_list_box out "App";
  Output.sep_list_box out (export_constr env) (m :: Array.to_list args);
  Output.close_list_box out ()

(** [let x : a = n in m] **)
and export_let_in env out x n a m =
  Output.open_box out "Lam";
  export_name env out x;
  Output.sep_box out ();
  export_constr env out n;
  Output.sep_box out ();
  export_constr env out a;
  Output.sep_box out ();
  export_constr (Environ.push_rel (x, Some(n), a) env) out m;
  Output.close_box out ()

(** [(m : a)] **)
and export_cast env out cast_kind m a =
  Output.open_box out "Lam";
  export_constr env out m;
  Output.sep_box out ();
  export_constr env out a;
  Output.close_box out ()

(** [match m as x in I args return p args x with b] **)
and export_case env out info p m b =
  Format.fprintf out "Case"

and export_fixpoint env out f =
  Format.fprintf out "Fix"

