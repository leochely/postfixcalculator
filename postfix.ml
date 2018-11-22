(*use "topfind"
require "str"
*)
let expression = "2 1 +";;
let p = (String.split_on_char ' ' expression)

let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ", "; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";;

print_list print_string p;;

let compute lst =
  let s = Stack.create in
    let rec process_elements = function
      | [] -> Stack.pop () s
      | "x"::t -> let temp = Stack.pop s () + Stack.pop s () in Stack.push temp s; process_elements t
      | "-"::t -> let temp = Stack.pop s () - Stack.pop s () in Stack.push temp s; process_elements t
      | "x"::t -> let temp = Stack.pop s () * Stack.pop s () in Stack.push temp s; process_elements t
      | "/"::t -> let temp = Stack.pop s / Stack.pop s in Stack.push temp s; process_elements t
      | x::t -> Stack.push a s
in compute p;;
