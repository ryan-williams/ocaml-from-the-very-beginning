
let n = int_of_string (Sys.argv.(1)) in
let l = ref [] in
for i = 1 to n do
  l := !l @ [i]
done ;
print_int (List.length !l) ;
print_newline()
