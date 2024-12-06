let string_to_rule str_rule = 
  let rule_split = String.split_on_char '|' str_rule in 
  let x = List.nth rule_split 0 in
  let y = List.nth rule_split 1 in
  (int_of_string(x),int_of_string(y))

let rule_to_string rule =
  match rule with (x,y) -> Printf.sprintf "(%d,%d)" x y

let string_to_update str_update =
  let strs = String.split_on_char ',' str_update in
  List.map int_of_string strs

let process_input filename =
  let file = open_in filename in
  let input = In_channel.input_all file in
  let split = Str.split (Str.regexp "\n\n") input in
  let rules = 
    match split with 
    | x :: _ -> List.map (string_to_rule) (String.split_on_char '\n' x)
    | _ -> raise (Failure "Invalid Input") in
    let updates = 
      match split with
      | _ :: x :: _ -> List.map (string_to_update) (String.split_on_char '\n' x)
      | _ -> raise (Failure "Invalid Input") in
  (rules, updates)

let rec check_update_with_rule update rule =
  match update with
  | h :: t -> if h == snd rule then not (List.mem (fst rule) t) else check_update_with_rule t rule
  | [] -> true 

let sum_mid lists =
  let mids = List.map (fun list -> List.nth list (List.length list / 2)) lists in
  List.fold_left (fun n acc -> n + acc) 0 mids