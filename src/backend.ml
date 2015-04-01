type data =
  | Bool of bool
  | Int of int
  | String of string
  | List of data list
  | Tuple of data list
  | Variant of string * data list
  | Record of (string * data) list

let bool b = Bool b
let int i = Int i
let string s = String s
let list l = List l
let tuple l = Tuple l
let variant c l = Variant (c, l)
let record l = Record l

module type S =
  sig
    val write : out_channel -> data -> unit
  end

module Json : S =
  struct
    let rec json_of_data data =
      match data with
      | Bool b -> `Bool b
      | Int i -> `Int i
      | String s -> `String s
      | List l -> `List (List.map json_of_data l)
      | Tuple l -> `Tuple (List.map json_of_data l)
      | Variant (c, l) -> `Variant (c, Some (`Tuple (List.map json_of_data l)))
      | Record l -> `Assoc (List.map (fun (k, v) -> (k, json_of_data v)) l)

    let write out data =
      let json = json_of_data data in
      Yojson.pretty_to_channel ~std:true out json
  end

module StrictJson : S =
  struct
    let rec json_of_data data =
      match data with
      | Bool b -> `Bool b
      | Int i -> `Int i
      | String s -> `String s
      | List l -> `List (List.map json_of_data l)
      | Tuple l -> `List (List.map json_of_data l)
      | Variant (c, l) -> `List (`String c :: List.map json_of_data l)
      | Record l -> `Assoc (List.map (fun (k, v) -> (k, json_of_data v)) l)

    let write out data =
      let json = json_of_data data in
      Yojson.Basic.pretty_to_channel ~std:true out json
  end
