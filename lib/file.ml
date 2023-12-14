exception FileNotFound of string

let read_file path = 
    if Bool.not @@ Sys.file_exists path then raise (FileNotFound path);
    let rec read_lines ic lines = 
        try
            let new_line  = input_line ic in
            read_lines ic (new_line :: lines)
        with End_of_file ->
            close_in ic;
            lines
    in
    read_lines (open_in path) []
