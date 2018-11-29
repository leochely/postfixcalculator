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
      | "*"::t -> let temp = Stack.pop s * Stack.pop s in Stack.push temp s; process_elements t
      | "/"::t -> let temp = Stack.pop s / Stack.pop s in Stack.push temp s; process_elements t
      | x::t -> Stack.push (int_of_string x) s; process_elements t
    in process_elements lst;;

let quit = false;;

print_string "Welcome to the OCaml PostFix Calculator\n";;
print_string "Type quit or q to exit\n";;
print_string "-----------------------------------------\nInput: ";;

let rec until_quit e =
    if (e = "quit" || e = "q") then (
        print_string "EXITING CALCULATOR\n"
    ) else (
        try 
            compute (String.split_on_char ' ' e);
            print_string "\nInput: ";
            until_quit (read_line ());
        with _ ->  
            print_string "Incorrect input";
            print_string "\nInput: ";
            until_quit (read_line ());
    );;
    
until_quit (read_line ());;
