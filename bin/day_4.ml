open Ocaml_aoc23

type card = { winners: int list; numbers: int list }

let parse_card str = 
    let parse_num_lst str =
        str
        |> String.trim
        |> String.split_on_char ' '
        |> List.filter (fun s -> s <> "")
        |> List.map int_of_string
    in
    match String.split_on_char ':' str with
    | _ :: nums :: [] -> 
            begin
                print_endline nums;
                match String.split_on_char '|' nums with
                | winners_str :: numbers_str :: [] -> Some { winners = parse_num_lst winners_str ; numbers = parse_num_lst numbers_str }
                | _ -> None
            end
    | _ -> None

let print_list l =
    let rec aux l =
        match l with
        | [] -> Printf.printf "]" ;
        | d :: [] -> begin
            Printf.printf "%d " d;
            aux []
        end
        | d :: tl -> begin
            Printf.printf "%d, " d;
            aux tl
        end
    in
    Printf.printf "[ " ;
    aux l

let print_card card =
    Printf.printf "Card: " ;
    print_list card.winners;
    Printf.printf " | " ;
    print_list card.numbers;
    Printf.printf "\n" 

let input_cards =
    let path = "data/input_4" in
    path
    |> File.read_file
    |> List.filter_map parse_card

(* ---------------------------- Part 1 ------------------------------- *)
let get_card_points card =
    let pow b e =
        let rec aux acc b e =
            match e with
            | 0 -> acc
            | 1 -> acc*b
            | _ -> aux (b*acc) b (e-1)
        in
        aux 1 b e
    in
    let { winners; numbers } = card in
    winners
    |> List.filter_map (fun w -> List.find_opt (fun n -> n = w) numbers)
    |> List.length
    |> fun i -> if i > 0 then 1 * (pow 2 (i-1)) else 0

let answer_part_1 = 
    input_cards
    |> List.map get_card_points
    |> List.fold_left (+) 0 

let () = List.iter print_card  input_cards
let () = Printf.printf "Sum of all points: %d\n" answer_part_1
let () = Printf.printf "Test: %d\n" (get_card_points {winners = [1; 4; 6] ; numbers = [ 21; 45; 1; 4; 6]})
