open Ocaml_aoc23

let use_msg = "day_1 <WORD>"

let path = ref ""
let input w = path := w

let () = Arg.parse [] input use_msg

let lines = File.read_file !path

let extract_nums acc c = 
    let is_num c = let code = Char.code c in code >= 48 && code <= 57 in
    if is_num c then
        acc ^ String.make 1 c
    else 
        acc

let form_calib_num num_str = 
    match String.length num_str with
    | 0 -> 0
    | 1 -> int_of_string @@ num_str ^ num_str
    | len -> int_of_string @@ (String.sub num_str 0 1) ^ (String.sub num_str (len - 1) 1)


let nums_str = List.map (String.fold_left extract_nums "") lines
let calib_list = List.map form_calib_num nums_str

let sum = List.fold_left (fun acc n -> acc + n) 0 calib_list
 
let () = Printf.printf "%d\n" sum

