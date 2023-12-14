open Ocaml_aoc23

let use_msg = "day_1 <WORD>"

let path = ref ""
let input w = path := w

let () = Arg.parse [] input use_msg

let lines = File.read_file !path

let contains_substring substr_tup str = 
    let (substr, label) = substr_tup in
    let sublen = String.length substr in
    let rec subfun pos finds str =
        match (substr, str ) with
        | ("", "") -> finds
        | (_, "") -> finds
        | _ -> 
                let len = String.length str in
                if len < sublen then 
                    finds
                else if substr.[0] = str.[0] && (String.equal substr (String.sub str 0 sublen)) then
                    subfun (pos + sublen) ( (pos, label) :: finds) (String.sub str sublen (len - sublen))
                else 
                    subfun (pos + 1) finds (String.sub str 1 (len - 1))
    in
    subfun 0 [] str
    

let contains_num str =
    let text_digits = [
        ("one", "1");
        ("two", "2");
        ("three", "3");
        ("four", "4");
        ("five", "5");
        ("six", "6");
        ("seven", "7");
        ("eight", "8");
        ("nine", "9");
        ("zero", "0");
        ("1", "1");
        ("2", "2");
        ("3", "3");
        ("4", "4");
        ("5", "5");
        ("6", "6");
        ("7", "7");
        ("8", "8");
        ("9", "9");
        ("0", "0")
    ] in
    text_digits
        |> List.map (fun a -> contains_substring a str)
        |> List.concat
        |> List.sort (fun a b -> (fst a) - (fst b))
        |> List.split
        |> snd
        |> String.concat ""

let nums_str = List.map contains_num lines

let form_calib_num num_str = 
    match String.length num_str with
    | 0 -> 0
    | 1 -> int_of_string @@ num_str ^ num_str
    | 2 -> int_of_string num_str
    | len -> int_of_string @@ (String.sub num_str 0 1) ^ (String.sub num_str (len - 1) 1)

let calib_list = List.map form_calib_num nums_str

let () = List.iter (Printf.printf "%d\n") calib_list

let sum = List.fold_left (fun acc n -> acc + n) 0 calib_list
 
let () = Printf.printf "%d\n" sum

