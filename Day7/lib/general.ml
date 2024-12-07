(* From https://stackoverflow.com/a/23456034 *)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let process_input filename =
  let lines = read_lines filename in 
  lines
  |> List.map (Str.split (Str.regexp ":* +"))
  |> List.map (List.map int_of_string)
  |> List.map (fun xs -> (List.hd xs, List.tl xs))


let apply_operations x list fns = List.concat_map (fun a -> List.map (fun fn -> fn a x) fns) list

let all_combinations list fns = 
  match list with
  | [] -> []
  | h :: t -> List.fold_left (fun acc v -> apply_operations v acc fns) [h] t