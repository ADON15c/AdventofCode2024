let processed_input = Day5.General.process_input "example_input.txt"
  
let rules = fst processed_input

let updates = snd processed_input

let check_update update =
  List.for_all (Day5.General.check_update_with_rule update) rules

let valid_updates =
  let valid u = if check_update u then Some u else None in
  List.filter_map valid updates

let output = Day5.General.sum_mid valid_updates

let () =
  Printf.printf "The answer is: %d" output 
