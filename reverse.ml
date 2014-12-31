
let rec write_lines_to_channel ch lines =
  match lines with
      [] -> ()
    | h::t -> output_string ch h ; output_string ch "\n" ; write_lines_to_channel ch t
;;

match Sys.argv with
  [| _; infile; outfile |] ->
    let in_channel = open_in infile in
    let in_lines = ref [] in
      try
        while true do
          in_lines := (input_line in_channel) :: !in_lines
        done ;
        close_in in_channel
      with
          End_of_file ->
            begin
              close_in in_channel ;
              let out_channel = open_out outfile in
              try
                write_lines_to_channel out_channel !in_lines ;
                close_out out_channel
              with _ ->
                close_out out_channel
            end
        | _ ->
            close_in in_channel
