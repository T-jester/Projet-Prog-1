open My_list
let string_of_list str_fun l = 
  let rec string_content = function
    | Nil -> ""
    | Cons(x,Nil)  -> (str_fun x)
    | Cons(x,l) -> (str_fun x) ^ ", " ^ (string_content l) 
  in "[" ^ (string_content l) ^ "]";;

let string_of_nat_list = string_of_list string_of_int in
let string_of_string_list = string_of_list (fun x -> x) in

let empty = Nil in
let one = Cons("a",Nil) in

let rec lst_to_alphal lst = match lst with
  |[] -> Nil
  | t::q -> Cons(t,lst_to_alphal q) in

let lst = lst_to_alphal [1; 3; 6; 10; 15; 21; 28; 36; 45; 55] in

let str_of_stroption = function None -> "" | Some s -> s in

let int_of_intoption = function None -> 0 | Some n -> n in

let alst_of_alstoption = function None -> Nil | Some l -> l in

let test_hd () = 
  Printf.printf "Tête de %s : %s.\n" (string_of_string_list one) (str_of_stroption (My_list.hd one));
  Printf.printf "Tête de %s : %d.\n\n" (string_of_nat_list lst) (int_of_intoption (My_list.hd lst))

in let test_tl () = 
  Printf.printf "Queue de %s : %s.\n" (string_of_string_list one) (string_of_string_list (alst_of_alstoption (My_list.tl one)));
  Printf.printf "Queue de %s : %s.\n\n" (string_of_nat_list lst) (string_of_nat_list (alst_of_alstoption (My_list.tl lst)))

in let test_length () = 
  Printf.printf "Taille de %s : %d.\n" (string_of_string_list one) (My_list.length one);
  Printf.printf "Taille de %s : %d.\n" (string_of_nat_list lst) (My_list.length lst);
  Printf.printf "Taille de %s : %d.\n\n" (string_of_string_list empty) (My_list.length empty)

in let test_map ()= 
  Printf.printf "Map de (x -> xx) sur %s : %s.\n" (string_of_string_list one) (string_of_string_list (My_list.map (fun s -> s ^ s) one));
  Printf.printf "Map de (x -> 2x) sur %s : %s.\n" (string_of_nat_list lst) (string_of_nat_list (My_list.map (fun n -> 2 * n) lst));
  Printf.printf "Map de (x -> 2x) sur %s : %s.\n\n" (string_of_nat_list empty) (string_of_nat_list (My_list.map (fun n -> 2 * n) empty));

in test_hd(); test_tl(); test_length(); test_map()
