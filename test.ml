
let rec pal l =
  match l with
      [] -> []
    | [a] -> [a]
    | h :: t -> h :: pal t @ [h];;

let rec rvr sf l = match l with
    h :: t -> rvr (h::sf) t
  | _ -> sf
;;

let rv l = rvr [] l;;

let rec rev l = match l with
    h :: t -> rev t @ [h]
  | _ -> l;;

let rec ispal l = rev l = l;;


let rec drop_last l = match l with
    f :: s :: t -> f :: (drop_last (s::t))
  | _ -> []
;;

let rec dlr sf l = match l with
    [] -> sf
  | [x] -> sf
  | f :: s :: t -> dlr (sf @ [f]) (s::t)
;;

let dl l = dlr [] l;;

let rec member e l = match l with
    [] -> false
  | h :: t -> h=e || member e t
;;

let rec make_set_r sf s = match s with
    [] -> sf
  | h :: t -> (if member h sf then make_set_r sf t else make_set_r (sf @ [h]) t)
;;

let make_set s = make_set_r [] s;;

let rec insert n s = match s with
    [] -> [n]
  | h :: t ->
      if n <= h
        then (n :: s)
        else h :: (insert n t)
;;

let rec isort s = match s with
    h :: t ->
      let st = isort t in
        (match st with
            [] -> [h]
          | hst :: tst ->
            if h <= hst
              then h :: st
              else hst :: (isort (h :: tst)))
  | _ -> s
;;


let rec merge a b = match a,b with
    [],l -> l
  | l,[] -> l
  | ha::ta,hb::tb ->
    if ha < hb
      then ha :: (merge ta b)
      else hb :: (merge a tb)
;;

let rec take_r sf n s = match n,s with
    0,_ | _,[] -> sf
  | _,h::t -> take_r (sf@[h]) (n-1) t
;;

let tk n s = take_r [] n s;;

let rec take n s = match n,s with
    0,l -> []
  | _,h::t -> h :: take (n-1) t
;;

let rec drop n s = match n,s with
    0,l -> l
  | _,h::t -> drop (n-1) t
;;

let rec len_r sf s = match s with [] -> sf | h::t -> len_r (sf+1) t;;
let len s = len_r 0 s;;

let rec length s = match s with [] -> 0 | h::t -> 1 + length t;;

let rec msort s = match s with
    [] -> s
  | [a] -> s
  | _ ->
    let l = length s / 2 in
      merge (msort (take l s)) (msort (drop l s))
;;

let rec mmap f l = match l with []->[] | h::t -> f h :: mmap f t;;

let double x = x*2;;

let calm l = mmap (fun ch -> if ch = '!' then '.' else ch) l;;

let rec apply f n a = match n with 0 -> a | _ -> apply f (n-1) (f a);;

let rec filter f l = match l with [] -> [] | h::t -> if f h then h :: (filter f t) else filter f t;;

let rec forall f l = match l with h::t -> f h && forall f t | _ -> true;;

let even x = x mod 2 = 0;;
let odd x = x mod 2 = 0;;

let rec mapl f a = match a with h :: t -> (mmap f h) :: (mapl f t) | _ -> [];;
let rec mapl f ls = mmap (mmap f) ls;;
let rec mapll f lls = mapl (mmap f) lls;;

exception Not_found;;

let rec smallest_r sf s = match s with
    [] -> if sf < 0 then raise Not_found else sf
  | h::t -> if (h > 0 && h < sf) || sf < 0 then smallest_r h t else smallest_r sf t
;;
let smallest s = smallest_r (-1) s;;

let smallest_or_zero s = try smallest s with Not_found -> 0;;


exception Bad_arg;;
let rec sqrt_r sf n = if sf*sf > n then (sf-1) else sqrt_r (sf+1) n;;
let sqrt_i n = if n < 0 then raise Bad_arg else sqrt_r 0 n;;
let safe_sqrt n = try sqrt_i n with Bad_arg -> 0;;

let fst (x,_) = x;;
let snd (_,y) = y;;

let num_keys d = length (make_set (mmap fst d));;

let rec replace k v d = match d with
    [] -> raise Not_found
  | (k',v')::t ->
    if k=k'
      then (k,v) :: t
      else (k',v') :: (replace k v t)
;;

let rec zip x y = match x,y with
    [],[] -> []
  | hx::tx, hy::ty -> (hx,hy) :: (zip tx ty)
  | _ -> raise (Invalid_argument "unequal lengths")
;;

let rec unzip d = (mmap fst d, mmap snd d);;

let rec has d k = match d with [] -> false | (k',v')::t -> if k=k' then true else has t k;;
let rec to_dict_r sf l = match l with
    [] -> sf
  | (k,v)::t -> if has sf k then to_dict_r sf t else to_dict_r ((k,v)::sf) t
;;
let to_dict l = to_dict_r [] l;;

let union a b = to_dict_r a b;;

let member_all x ls = forall (member x) ls;;

let truncate n ls = mmap (tk n) ls;;

let head l = match l with [] -> raise Not_found | h::t -> h;;
let hd d l = try head l with Not_found -> d;;
let firsts d ls = mmap (hd d) ls;;

type rect = Square of int | Rect of int*int;;
let area r = match r with Square(n) -> n*n | Rect(a,b) -> a*b;;

let rotate r = match r with Square(_) -> r | Rect(w,h) -> if w > h then Rect(h,w) else r;;

let rec mergec cmp a b = match a,b with
    [],l -> l
  | l,[] -> l
  | ha::ta,hb::tb ->
    if cmp ha hb
      then ha :: (merge ta b)
      else hb :: (merge a tb)
;;

let rec msortc cmp s = match s with
    [] -> s
  | [a] -> s
  | _ ->
    let l = length s / 2 in
      mergec cmp (msortc cmp (take l s)) (msortc cmp (drop l s))
;;

let width r = match r with Square(n) -> n | Rect(w,_) -> w;;

let wcmp r1 r2 = width r1 < width r2;;

let rsort = msortc wcmp;;

type 'a sequence = Nil | Cons of 'a * 'a sequence;;

let rec stake_r sf n s = match n,s with 0,_|_,Nil -> sf | _,Cons(h,t) -> stake_r (Cons(h,sf)) (n-1) t;;
let stake n s = stake_r Nil n s;;

let rec sdrop n s = match n,s with 0,_|_,Nil -> s | _,Cons(h,t) -> sdrop (n-1) t;;

let rec smap f s = match s with Nil -> Nil | Cons(h,t) -> Cons(f h, smap f t);;

type 'a option = None | Some of 'a;;

type expr =
    Num of int
  | Add of expr*expr
  | Sub of expr*expr
  | Mult of expr*expr
  | Div of expr*expr
  | Pow of expr*expr
;;

let rec pow b e = match e with
    0 -> 1
  | 1 -> b
  | _ -> (pow b (e/2)) * (pow b (e-e/2))
;;

let rec evaluate e = match e with
    Num(n) -> Some(n)
  | Add(e1,e2) -> (match evaluate e1, evaluate e2 with None,_|_,None -> None | Some(e1),Some(e2) -> Some(e1+e2))
  | Sub(e1,e2) -> (match evaluate e1, evaluate e2 with None,_|_,None -> None | Some(e1),Some(e2) -> Some(e1-e2))
  | Mult(e1,e2) -> (match evaluate e1, evaluate e2 with None,_|_,None -> None | Some(e1),Some(e2) -> Some(e1*e2))
  | Div(e1,e2) -> (match evaluate e1, evaluate e2 with None,_|_,None -> None | _,Some(0) -> None | Some(e1),Some(e2) -> Some(e1/e2))
  | Pow(e1,e2) -> (match evaluate e1, evaluate e2 with None,_|_,None -> None | Some(e1),Some(e2) -> Some(pow e1 e2))
;;

(* Chapter 11 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec has e t = match t with Lf -> false | Br(e',l,r) -> if e=e' then true else has e l || has e r;;
let rec flip t = match t with Lf -> Lf | Br(x,l,r) -> Br(x,flip r,flip l);;

let rec same_shape t1 t2 = match t1,t2 with Lf,Lf -> true | Lf,_|_,Lf -> false | Br(_,l1,r1),Br(_,l2,r2) -> same_shape l1 l2 && same_shape r1 r2;;

let rec insert cmp t e = match t with
    Lf -> Br(e,Lf,Lf)
  | Br(x,l,r) ->
    if e=x
      then t
      else if cmp e x
        then Br(x,insert cmp l e, r)
        else Br(x,l,insert cmp r e)
;;

let rec tree_of_list cmp l = match l with
    [] -> Lf
  | h::t -> insert cmp (tree_of_list cmp t) h;;

let rec tree_map f t = match t with Lf -> Lf | Br(x,l,r) -> Br(f x,tree_map f l, tree_map f r);;

let rec tm t2 f t = match t with Lf -> t2 | Br(x,l,r) -> tm (tm (f t2 x) f l) f r;;

let tree_union t1 t2 = tm t1 (insert ( < )) t2;;

type 'a mtree = L | B of 'a * ('a mtree) list;;

let rec sum l = match l with [] -> 0 | h::t -> h + (sum t);;
let rec size mt = match mt with L -> 0 | B(_,cs) -> 1 + sum (mmap size cs);;
let rec total mt = match mt with L -> 0 | B(x,cs) -> x + sum (mmap total cs);;
let rec mt_map f mt = match mt with L -> L | B(x,cs) -> B(f x, mmap (mt_map f) cs);;

(* Chapter 12 *)

let rec pp_r first l =
  match l with
      [] -> print_string "]"
    | h::t ->
        print_string (if first then "[" else ";") ;
        print_string (string_of_int h) ;
        pp_r false t
;;
let pp l = pp_r true l;;

let rec read_three () =
  try
    let x = read_int () in
      let y = read_int() in
        let z = read_int() in
          (x,y,z)
  with Failure "int_of_string" ->
    print_string "try again" ;
    print_newline () ;
    read_three ()
;;

let rec read_dict_entry () =
  try
    print_string "Enter key: " ;
    match read_line () with
        "" -> None
      | s -> let n = int_of_string s in
        print_string "Enter value: " ;
        let s = read_line () in
          Some ((n,s))
  with Failure "int_of_string" ->
    print_string "try again" ;
    print_newline () ;
    read_dict_entry ()
;;

let rec read_dict () = match read_dict_entry () with
    None -> []
  | Some(k,v) -> (k,v) :: read_dict ()
;;

let rec table_line ch r c n =
  print_int r ; print_string " " ; print_int c ; print_string " " ; print_int n ; print_newline() ;
  if c < n then
    (if c > 0 then output_string ch "\t" ;
    output_string ch (string_of_int ((r+1) * (c+1)) ) ;
    table_line ch r (c+1) n)
;;

let rec table_r ch r n =
  print_int r ; print_newline () ;
  if r < n then
    (table_line ch r 0 n ;
    output_string ch "\n" ;
    table_r ch (r+1) n)
;;

let rec table filename n =
  let ch = open_out filename in
    table_r ch 0 n ;
    close_out ch
;;

let rec wcl_r ch num =
  try
    input_line ch ;
    wcl_r ch (1+num)
  with End_of_file -> num
;;

let rec wcl filename =
  let ch = open_in filename in
    let num = wcl_r ch 0 in
      close_in ch ;
      num
;;

let rec copy_channel ach bch =
  try
    let line = input_line ach in
      output_string bch line ;
      output_string bch "\n" ;
      copy_channel ach bch
  with End_of_file -> ()
;;

let copy_file a b =
  let ach = open_in a in
    let bch = open_out b in
      copy_channel ach bch ;
      close_out bch ;
      close_in ach
;;


(* Chapter 13 *)

let sum_array a =
  let sum = ref 0 in
    print_int (Array.length a) ; print_newline() ;
    for i = 0 to Array.length a - 1 do
      print_int i ; print_newline () ;
      sum := !sum + a.(i)
    done ;
    !sum
;;
sum_array [|1;2;3|];;

let rev_array a =
  let n = Array.length a in
    for i = 0 to (n + 1) / 2 - 1 do
      let t = a.(i) in
        a.(i) <- a.(n-1-i) ;
        a.(n-1-i) <- t
    done ;
    a
;;
rev_array [|1;2;3|];;

let mult_table n =
  let t = Array.make n [||] in
    for r = 1 to n do
      t.(r-1) <- (Array.make n 1) ;
      for c = 1 to n do
        (t.(r-1)).(c-1) <- r*c
      done
    done ;
    t
;;
mult_table 5;;

let lower c = match c with
    'A'..'Z' -> char_of_int (int_of_char c + 32)
  | _ -> c
;;

let upper c = match c with
    'a'..'z' -> char_of_int (int_of_char c - 32)
  | _ -> c
;;

let slice a f t =
  let r = Array.make (t-f) a.(f) in
    for i = f to (t-1) do
      r.(i-f) <- a.(i)
    done ;
    r
;;

let array_sliding n a =
  let l = Array.length a in
  let r = Array.make (l - n + 1) [||] in
  for i = 0 to l - n do
    r.(i) <- (slice a i (i+n))
  done ;
  r
;;

let channel_statistics in_channel =
  let lines = ref 0 in
  let spaces = ref 0 in
  try
    while true do
      let line = input_line in_channel in
        lines := !lines + 1 ;
        for i = 0 to String.length line - 2 do
          if String.get line i = ' ' && String.get line (i+1) != ' ' then
            spaces := !spaces + 1
        done
    done
  with End_of_file ->
    print_int !lines ;
    print_string " lines" ;
    print_newline () ;
    print_int !spaces ;
    print_string " spaces" ;
    print_newline ()
;;

let file_statistics filename =
  let channel = open_in filename in
    try
      channel_statistics channel ;
      close_in channel
    with
      _ -> close_in channel
;;

let round f =
  if f -. (floor f) >= 0.5 then int_of_float (floor f) + 1 else int_of_float (floor f);;

let tri_point (x1,y1) (x2,y2) =
  let sign = if (x1-.x2 > 0.) = (y1-.y2 > 0.) then -1. else 1. in
  let dx = if x1 > x2 then x1-.x2 else x2-.x1 in
  let dy = if y1 > y2 then y1-.y2 else y2-.y1 in
  ((x1 +. x2 +. (sqrt 3.) *. dy) /. 2., (y1 +. y2 +. (sqrt 3.) *. dx *. sign) /. 2.)
;;

let sep f = (floor f, f -. (floor f));;

let star f = let spaces = int_of_float (floor (f*.50.)) in for i = 1 to spaces - 1 do print_string " " done ; print_string "*" ; print_newline () ;;

let pi = 4.0 *. atan 1.;;
let plot f a b step =
  let i = ref a in
  while !i < b do
    star (f !i) ;
    i := !i +. step
  done
;;

let rec lconcat_r sf ls = match ls with [] -> sf | h::t -> lconcat_r (sf @ h) t ;;
let lconcat ls = lconcat_r [] ls;;

let rec list_mems ls = forall (List.mem true) ls;;

let sfilter f s =
  let accum = ref [] in
  String.iter (fun c -> if f c then accum := (String.make 1 c) :: !accum) s ;
  String.concat "" (rev !accum)
;;

let scount f s = String.length (sfilter f s);;

let calm = String.map (fun c -> if c = '!' then '.' else c);;
let joyn = String.concat "";;

let joyn ls =
  let b = Buffer.create 10 in
  List.map (fun s -> print_string s; print_newline(); Buffer.add_string b s) ls ;
  Buffer.contents b
;;

let rec count_r sf idx haystack needle =
  if idx + (String.length needle) > String.length haystack
    then sf
    else count_r (sf + (if String.compare (String.sub haystack idx (String.length needle)) needle = 0 then 1 else 0)) (idx + 1) haystack needle
;;
let count = count_r 0 0;;
