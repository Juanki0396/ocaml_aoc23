open Ocaml_aoc23


type color = Red of int | Green of int | Blue of int
type subset = { red: int; green: int; blue: int }
type game = { id: int; subsets: subset list }

(* Parse the string "1 red" of the input file into a color variant *)
let parse_color color_str = 
    match String.split_on_char ' ' (String.trim color_str) with
    | n :: "red" :: [] when Option.is_some (int_of_string_opt n) -> Some (Red (int_of_string n))
    | n :: "green" :: [] when Option.is_some (int_of_string_opt n) -> Some (Green (int_of_string n))
    | n :: "blue" :: [] when Option.is_some (int_of_string_opt n) -> Some (Blue (int_of_string n))
    | _ -> None

(* Parse the string "Game 12" of the input file into an int containing gameId *)
let parse_game_id game_str = 
    match String.split_on_char ' ' (String.trim game_str) with
    | "Game" :: n :: [] when Option.is_some (int_of_string_opt n) -> Some (int_of_string n)
    | _ -> None

(* Create a subset record from a list of colors by summing each color int *)
let create_subset color_lst = 
    let rec aux r g b lst =
        match lst with
        | Red i :: tl -> aux (r+i) g b tl
        | Green i :: tl -> aux r (g+i) b tl
        | Blue i :: tl -> aux r g (b+i) tl
        | [] ->  { red = r; green = g; blue = b }
    in
    aux 0 0 0 color_lst

(* Parse the string "1 red, 3 blue, 2 green" of the input file *)
let parse_subset str =
    str
    |> String.trim
    |> String.split_on_char ',' 
    |> List.filter_map parse_color
    |> create_subset

(* Parse the string "Game 13: 1 red, 3 blue, 2 gree; 2 blue, 8 red" of the input file into a game record *)
let parse_line line = 
    let splited_str = String.split_on_char ':' line in
    match splited_str with
    | game_str :: subsets_str :: [] -> begin
        let game_id = parse_game_id game_str in
        let subsets = List.map parse_subset (String.split_on_char ';' subsets_str) in
        match game_id with
            | Some id -> Ok { id = id; subsets = subsets }
            | None -> Error "Invalid game line" 
    end
    | _ -> Error "Invalid game line"

let game_list = 
    let path = "data/input_2" in
    path
    |> File.read_file
    |> List.filter_map (fun line -> Result.to_option (parse_line line))

(* ----------------------- Part 1 -------------------- *)

let is_valid_subset limit tested = 
    tested.red <= limit.red && tested.green <= limit.green && tested.blue <= limit.blue

let is_game_valid limit game = 
    List.for_all (is_valid_subset limit) game.subsets

let max_subset = {
    red = 12 ;
    green = 13 ;
    blue = 14 
}

let sum_valid_id game_lst =
    game_lst
    |> List.filter (is_game_valid max_subset)
    |> List.fold_left (fun acc a -> acc + a.id) 0

let answer_part_1 = sum_valid_id game_list

let () = Printf.printf "Part 1 - The sum of valid games: %d\n" answer_part_1

(* ----------------------- Part 2 -------------------- *)

let minimum_set subset_lst = 
    let max a b = if ( a - b ) >= 0 then a else b in
    let rec aux r g b lst = 
        match lst with
        | [] ->  { red = r; green = g; blue = b }
        | {red ; green; blue} :: tl -> aux (max r red) (max g green) (max b blue) tl
    in
    aux 0 0 0 subset_lst

let game_power game = 
    let {red; green; blue} = minimum_set game.subsets in
    red * green * blue

let answer_part_2 = List.fold_left (fun acc game -> acc + (game_power game)) 0 game_list

let () = Printf.printf "Part 2 - The sum of powers of minimum cube sets: %d\n" answer_part_2
