open Ocaml_aoc23

module type Mapper_type = sig
    type map_range
    type mapper = map_range list
    val map_range_of_string : string -> map_range
    val full_map : mapper -> int -> int
end

module Mapper : Mapper_type = struct
    type map_range = {
        source_start : int;
        dest_start : int;
        range : int
    }
    type mapper = map_range list
    let map_range_of_string s =
        let int_lst = 
            s
            |> String.trim
            |> String.split_on_char ' '
            |> List.map int_of_string
        in
        match int_lst with
        | s :: d :: r :: [] -> { source_start = s ; dest_start = d ; range = r }
        | _ -> failwith "Invalid input"
    let map m x =
        let diff = x - m.source_start in
        if diff < 0  || diff > (m.range - 1) then
            None
        else 
            Some (m.dest_start + diff)
    let full_map m_lst x =
        let mapped = List.filter_map (fun m -> map m x) m_lst in
        match mapped with
        | y :: _ -> y
        | _ -> x
end

type seed_map = { seeds: int list; maps: Mapper.mapper list }

let parse_seeds str =
    match String.split_on_char ':' str with
    | "seeds" :: n :: [] -> begin
        n
        |> String.trim
        |> String.split_on_char ' '
        |> List.map int_of_string
    end
    | _ -> []

let parse_input str_lst = 
    let rec aux acc_1 acc_2 seeds str_lst =
        match str_lst with
        | [] -> { seeds = seeds; maps = acc_1 }
        | "" :: lst -> aux (acc_2 :: acc_1) [] seeds lst
        | line :: lst when String.starts_with ~prefix:"seeds:" line -> aux acc_1 acc_2 (parse_seeds line) lst
        | line :: lst when String.ends_with ~suffix:":" line -> aux acc_1 acc_2 seeds lst
        | line :: lst -> let map_range = Mapper.map_range_of_string line in aux acc_1 (map_range :: acc_2) seeds lst
    in
    aux [] [] [] str_lst

let rec apply_multiple_mappers mapper_lst x = 
    match mapper_lst with
    | m :: lst -> apply_multiple_mappers lst (Mapper.full_map m x)
    | [] -> x

let seed_map_1 = 
    let path = "data/input_5" in
    path
    |> File.read_file
    |> parse_input

let answer_1 = 
    let { seeds; maps } = seed_map_1 in
    let rec min acc lst = 
        match lst with
        | hd :: tl -> min (if hd < acc then hd else acc) tl
        | [] -> acc
    in
    seeds
    |> List.map (apply_multiple_mappers maps)
    |> min (100000000000)

let () = Printf.printf "The lowest location is %d\n" answer_1


