type 'a my_list = Nil | Cons of 'a * 'a my_list;;

let string_of_list str_fun l = 
  let rec string_content = function
    | Nil -> ""
    | Cons(x,Nil)  -> (str_fun x)
    | Cons(x,l) -> (str_fun x) ^ ", " ^ (string_content l) 
  in "[" ^ (string_content l) ^ "]";;



let hd list = match list with
  | Nil -> None
  | Cons(a,_) -> Some(a);;



let tl list = match list with
  | Nil -> None
  | Cons(_,rest_list) -> Some(rest_list);;



let rec length list = match list with
  | Nil -> 0
  | Cons(_,rest_list) -> 1 + length rest_list;;



let rec map f list = match list with
  | Nil -> Nil
  | Cons(a,rest_list) -> Cons(f a, map f rest_list);;

