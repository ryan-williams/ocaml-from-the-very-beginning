
let rec count_r sf idx haystack needle =
  if idx + (String.length needle) > String.length haystack
    then sf
    else count_r (sf + (if String.compare (String.sub haystack idx (String.length needle)) needle = 0 then 1 else 0)) (idx + 1) haystack needle
;;
let count = count_r 0 0;;

match Sys.argv with
  [| _; needle; haystack |] ->
    let ch = open_in haystack in
    try
      while true do
        let line = input_line ch in
        if count line needle > 0 then
          begin
            print_string line ;
            print_newline ()
          end
      done
    with
      _ ->
        close_in ch
