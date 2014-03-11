(** Pretty printing output utilities *)

(** To print an object as "name(arg1, ..., argn)" do
      
    open_box "name";
    export arg1;
    sep_box ();
    ...;
    sep_box();
    export argn;
    close_box ()
    
    The pretty printing is done with [hov] boxes to achieve a good
    balance between compactness and readability. **) 

let open_box out name =
  Format.fprintf out "@[<hov1>%s(@," name

let sep_box out () =
  Format.fprintf out ",@ "

let close_box out () =
  Format.fprintf out ")@]"

(** The list_box variant "name[arg1; ...; argn]" will always be either
    completely horizontal or completely vertical (good for lists). **)

let open_list_box out name =
  Format.fprintf out "@[<hv1>%s[@," name

let rec sep_list_box out f xs =
  match xs with
  | [] -> ()
  | [x] ->
    Format.fprintf out "%a" f x
  | x :: xs ->
    Format.fprintf out "%a;@ " f x;
    sep_list_box out f xs

let close_list_box out () =
  Format.fprintf out "]@]"

