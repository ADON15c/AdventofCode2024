let processed_input = Day5.General.process_input "input.txt"
let rules = fst processed_input
let updates = snd processed_input

let compare_with_rules x y = 
  if x==y then 0 else
  if List.mem (x,y) rules then -1 else 1

let sorted_updates =  List.map (List.sort compare_with_rules) updates

let changed_updates = List.fold_left2 (fun acc x y -> if (List.equal (fun x1 x2 -> x1==x2) x y) then acc else y::acc) [] updates sorted_updates

let output = Day5.General.sum_mid changed_updates

let () = Printf.printf "The answer is: %d" output 