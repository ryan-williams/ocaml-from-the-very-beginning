(* Command line text file statistics program *)
try
  begin
    match Sys.argv with
        [|_; filename|] ->
          let stats = Teststat.stats_from_file filename in
          print_string "Words: ";
          print_int (Teststat.words stats);
          print_newline ();
          print_string "Characters: ";
          print_int (Teststat.characters stats);
          print_newline ();
          print_string "Sentences: ";
          print_int (Teststat.sentences stats);
          print_newline ();
          print_string "Lines: ";
          print_int (Teststat.lines stats);
          print_newline ();
          print_string "Hist: ";
          print_newline ();
          let hist = Teststat.hist stats in
          for i = 1 to Array.length hist do
            if (hist.(i-1) > 0) then
              begin
                print_string "\t" ;
                print_char (char_of_int (i-1)) ;
                print_string ": " ;
                print_int hist.(i-1) ;
                print_newline ()
              end
          done
      | _ ->
          print_string "Usage: stats <filename>";
          print_newline ()
  end
with
  e ->
    print_string "An error occurred: ";
    print_string (Printexc.to_string e);
    print_newline ();
    exit 1
