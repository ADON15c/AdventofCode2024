let input = Day7.General.process_input "input.txt"

let output =
  let fold_func acc p =
    let combinations = Day7.General.all_combinations (snd p) in
    if (List.mem (fst p) combinations) then acc + (fst p) else acc in
  List.fold_left fold_func 0 input

let () =
  Printf.printf "The answer is: %d" output 
