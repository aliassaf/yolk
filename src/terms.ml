(** Export of Coq terms **)

module B = Backend
module T = Term

(** The name [x] of a local variable (used in lambdas, products,
    and letins) to be pushed in [env] **)
let name env x =
  match x with
  | Names.Name(id) -> B.variant "Name" [B.String (Names.string_of_id id)]
  | Names.Anonymous -> B.variant "Anonymous" []

(** The sorts of the pCIC are either Prop or Type i.
    The actual implementation is a bit different:
    - [Prop(Null)] corresponds to Prop
    - [Prop(Pos)] actually corresponds to Type 0 (probably a remnant
      of the impredicative Set in the original CIC)
    - [Type(u)] is an algebraic universe and can even correspond
      to Prop for some values of [u]. **)
let sorts env s =
  begin match s with
  | T.Prop(T.Null) -> Sorts.prop env
  | T.Prop(T.Pos) -> Sorts.type0 env
  | T.Type(u) -> Sorts.universe env u
  end

(** The terms of the pCIC **)
let rec constr env m =
  match Term.kind_of_term m with
  | T.Rel(i) ->
    B.variant "Rel" [B.Int i]
  | T.Var(_) -> Error.not_supported "Var"
  | T.Meta(_) -> Error.not_supported "Meta"
  | T.Evar(_) -> Error.not_supported "Evar"
  | T.Sort(s) ->
    B.variant "Sort" [sorts env s]
  | T.Cast(m, cast_kind, a) ->
    B.variant "Cast" [
      constr env m;
      constr env a;
      ]
  | T.Prod(x, a, b) ->
    B.variant "Prod" [
      name env x;
      constr env a;
      constr (Environ.push_rel (x, None, a) env) b;
      ]
  | T.Lambda(x, a, m) ->
    B.variant "Lambda" [
      name env x;
      constr env a;
      constr (Environ.push_rel (x, None, a) env) m;
      ]
  | T.LetIn(x, n, a, m) ->
    B.variant "LetIn" [
      name env x;
      constr env a;
      constr (Environ.push_rel (x, Some(n), a) env) m;
      ]
  | T.App(m, args) ->
    B.variant "App" [
      constr env m;
      B.List (List.map (constr env) (Array.to_list args));
      ]
  | T.Const(c) ->
    B.variant "Const" [B.String (Names.string_of_con c)]
  | T.Ind((mind, i)) ->
    B.variant "Ind" [
      B.String (Names.string_of_mind mind);
      B.Int i]
  | T.Construct(((mind, i), j)) ->
    B.variant "Construct" [
      B.String (Names.string_of_mind mind);
      B.Int i;
      B.Int j;
      ]
  | T.Case(info, p, m, b) ->
    let mind, i = info.T.ci_ind in
    B.variant "Case" [
      B.String (Names.string_of_mind mind);
      B.Int i;
      constr env p;
      constr env m;
      B.List (List.map (constr env) (Array.to_list b));
      ]
  | T.Fix((ks, i), (fs, bs, ms)) ->
    B.variant "Fix" [
      B.List (List.map (fun i -> B.Int i) (Array.to_list ks));
      B.Int i;
      B.List (List.map (name env) (Array.to_list fs));
      B.List (List.map (constr env) (Array.to_list bs));
      B.List (List.map (constr env) (Array.to_list ms));
      ]
  | T.CoFix(i, (fs, bs, ms)) ->
    B.variant "CoFix" [
      B.Int i;
      B.List (List.map (name env) (Array.to_list fs));
      B.List (List.map (constr env) (Array.to_list bs));
      B.List (List.map (constr env) (Array.to_list ms));
      ]

(** A local context is a list of triples [(x, m, a)] where:
    - [x] is the original name of the variable (can be alpha-renamed),
    - [m] is either [None] (for lambda and product variables) or
      the definition of the variable (for let in variables),
    - [a] is the type of the variable. *)
let rel_context env ctx =
  let entry env (x, m, a) =
    B.List [
      name env x;
      begin
        match m with
        | None -> B.variant "None" []
        | Some(m) -> B.variant "Some" [constr env a]
      end;
      constr env a;
    ]
  in
  let rec entries env xs =
    match xs with
    | [] -> []
    | xma :: ys ->
      entry env xma :: entries (Environ.push_rel xma env) ys
  in
  B.List (entries env (List.rev ctx))
