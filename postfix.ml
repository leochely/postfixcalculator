let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ", "; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";;

let compute lst =
  let s = Stack.create () in
    Stack.push 0 s;
    let rec process_elements = function
      | [] -> let result = Stack.pop s in print_int result
      | "+"::t -> let temp = Stack.pop s + Stack.pop s in Stack.push temp s; process_elements t
      | "-"::t -> let temp = Stack.pop s - Stack.pop s in Stack.push temp s; process_elements t
      | "x"::t -> let temp = Stack.pop s * Stack.pop s in Stack.push temp s; process_elements t
      | "/"::t -> let temp = Stack.pop s / Stack.pop s in Stack.push temp s; process_elements t
      | x::t -> Stack.push (int_of_string x) s; process_elements t
    in process_elements lst;;

print_string "Welcome to the OCaml PostFix Calculator\n";;
print_string "-----------------------------------------\n";;

let quit = false;;

while quit != true do
    print_string "Input: ";
    let expression = read_line ();
    if expression == "quit" || exression == "q" then 
        quit = true
    else
        compute (String.split_on_char ' ' expression);
    print_string "\n";
done;;
