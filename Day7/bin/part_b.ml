let input = Day7.General.process_input "input.txt"

let operations = [
  (fun x y -> x + y);
  (fun x y -> x * y);
  (fun x y -> int_of_string ((string_of_int x) ^ (string_of_int y)))]
let output =
  let fold_func acc p =
    let combinations = Day7.General.all_combinations (snd p) operations in
    if (List.mem (fst p) combinations) then acc + (fst p) else acc in
  List.fold_left fold_func 0 input

let () =
  Printf.printf "The answer is: %d" output 
