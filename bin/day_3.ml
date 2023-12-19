open Ocaml_aoc23

type motor_plane = Symbol | Number | Separator

let char_to_motor c = 
    match c with 
    | '0' .. '9' -> Number
    | '.' -> Separator
    | _ -> Symbol

let check line pos = 
    pos < String.length line && 
    pos >= 0 &&
    Symbol = char_to_motor @@ String.get line pos

let check_for_symbols line start len = 
    let rec aux s l r =
        if l <= 0 then
            r
        else
            aux (s+1) (l-1) (r || check line s)
    in 
    aux start len false

let next_num line start =
    let r = Str.regexp {|[0-9]+|} in
    let pos = Str.search_forward r line start in
    let num = Str.matched_string line in
    (num, pos)

let parse_schema_line line prev next num_lst = 
    let rec aux pos num_lst =
        try
            let n, start = next_num line pos in
            let len = String.length n in
            if check_for_symbols prev (start-1) (len + 2) || 
            check_for_symbols line (start-1) (len + 2) || 
            check_for_symbols next (start-1) (len + 2) then
                aux (start + len) (n::num_lst)
            else
                aux (start + len) num_lst
        with Not_found -> num_lst
    in
    aux 0 num_lst

let parse_whole_schema schema = 
    let rec aux prev schema nums i = 
        match schema with
        | line :: next :: new_schema -> begin
            let new_nums = parse_schema_line line prev next nums in
            aux line (next::new_schema) new_nums (i+1)
        end
        | line :: [] -> begin 
            let new_nums = parse_schema_line line prev "" nums in
            aux line [] new_nums (i+1)
        end
        | [] -> nums
    in
    aux "" schema [] 0

let lines = 
    let path = "data/input_3" in
    List.rev @@ File.read_file path

let answ_part1 =
    lines
        |> parse_whole_schema
        |> List.map int_of_string
        |> List.fold_left (+) 0

let () = print_int answ_part1
