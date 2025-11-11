(******************************)
(*** For debugging purposes ***)
(******************************)

(* print out an integer list *)
let rec print_int_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_int x; print_newline ()
  | x :: xs -> print_int x; print_string "; "; print_int_list xs

(* print out a string list *)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_string x; print_newline ()
  | x :: xs -> print_string x; print_string "; "; print_string_list xs


(********************)
(* Problem 1: pow *)
(********************)

let rec pow x p =
  if p = 0 then 1
  else x * pow x (p - 1)

(********************)
(* Problem 2: range *)
(********************)

let rec range num1 num2 =
  if num2 < num1 then []
  else num1 :: range (num1 + 1) num2

(**********************)
(* Problem 3: flatten *)
(**********************)

let rec flatten l =
  match l with
  | [] -> []
  | head :: tail -> head @ (flatten tail)

(*****************************)
(* Problem 4: remove_stutter *)
(*****************************)

let rec remove_stutter l =
  match l with
  | [] -> []
  | [x] -> [x]
  | first :: (second :: _ as tail) ->
      if first = second then remove_stutter tail
      else first :: remove_stutter tail

(*********************)
(* Problem 5: rotate *)
(*********************)

let rotate l n =
  let rec splitAt l k =
    if k = 0 then ([], l)
    else match l with
    | [] -> ([], [])
    | x::xs ->
      let (left, right) = splitAt xs (k-1) in
      (x::left, right)
  in
  let len = List.length l in
  if len = 0 then []
  else
    let m = n mod len in
    let k = (len - m) mod len in
    let (left, right) = splitAt l k in
    right @ left

(*****************************)
(* Problem 6: jump *)
(*****************************)
let jump lst1 lst2 =
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  let k = if len1 < len2 then len1 else len2 in
  let rec build i acc =
    if i > k then List.rev acc
    else
      let elem =
        if i mod 2 = 1 then
          let rec nth l n = match l, n with
            | [], _ -> raise (Failure "nth")
            | x::_, 0 -> x
            | _::xs, m -> nth xs (m-1)
          in nth lst2 (i-1)
        else
          let rec nth l n = match l, n with
            | [], _ -> raise (Failure "nth")
            | x::_, 0 -> x
            | _::xs, m -> nth xs (m-1)
          in nth lst1 (i-1)
      in build (i+1) (elem::acc)
  in build 1 []


(*****************************)
(* Problem 7: nth (every n-th element) *)
(*****************************)
let nth l n =
  if n <= 0 then raise (Failure "nth")
  else
    let rec aux i l acc =
      match l with
      | [] -> List.rev acc
      | x::xs ->
         if i mod n = 0 then aux (i+1) xs (x::acc)
         else aux (i+1) xs acc
    in
    aux 1 l []

(*****************************************************)
(* Problem 8: Digital Roots and Additive Persistence *)
(*****************************************************)

let rec digitsOfInt n =
  if n < 10 then [n]
  else digitsOfInt (n / 10) @ [n mod 10]

(*additive Persistence *)

let additivePersistence n =
  let rec sumList l = match l with
    | [] -> 0
    | x::xs -> x + sumList xs
  in
  let rec helper count x =
    if x < 10 then count
    else
      let s = sumList (digitsOfInt x) in
      helper (count + 1) s
  in helper 0 n

(*Digital Root*)
let digitalRoot n =
  let rec sumList l = match l with
    | [] -> 0
    | x::xs -> x + sumList xs
  in
  let rec helper x =
    if x < 10 then x
    else helper (sumList (digitsOfInt x))
  in helper n

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for pow *)
  let _ =
    try
      assert (pow 3 1 = 3);
      assert (pow 3 2 = 9);
      assert (pow (-3) 3 = -27)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for rotate *)
  let _ =
    try
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 2 = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 0 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 7 = ["b"; "c"; "d"; "e"; "f"; "g"; "h"; "a"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for jump *)
  let _ =
    try
      assert (jump ["first"; "second"; "third"; "fourth"] ["fifth"; "sixth"; "seventh"; "eighth"] = ["fifth"; "second"; "seventh"; "fourth"]);
      assert (jump [1; 3; 5; 7] [0; 2; 4; 6; 8] = [0; 3; 4; 7]);
      assert (jump ["a"; "b"] ["c"] = ["c"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for nth *)
  let _ =
    try
      (*print_int_list (nth [1; 2; 3; 4; 5; 6; 7] 1);*)
      assert (nth [1; 2; 3; 4; 5; 6; 7] 1 = [1; 2; 3; 4; 5; 6; 7]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 2 = [2; 4; 6]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 3 = [3; 6])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitsOfInt *)
  let _ =
    try
      assert (digitsOfInt 3124 = [3;1;2;4]);
      assert (digitsOfInt 352663 = [3;5;2;6;6;3]);
      assert (digitsOfInt 31243 = [3;1;2;4;3]);
      assert (digitsOfInt 23422 = [2;3;4;2;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for additivePersistence *)
  let _ =
    try
      assert (additivePersistence 9876 = 2)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitalRoot *)
  let _ =
    try
      assert (digitalRoot 9876 = 3)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 10 programming questions are correct.\n") (10 - !error_count)

let _ = main()
