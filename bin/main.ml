let () =
  let open Parse in
  let filename = Sys.argv.(1) in
  let tokens = parse filename in
  List.iter (fun t ->
    match t with
      | T_INC -> print_string "INC "
      | T_DEC -> print_string "DEC "
      | T_PLUS -> print_string "PLUS "
      | T_MINUS -> print_string "MINUS "
      | T_DOT -> print_string "DOT "
      | T_COMMA -> print_string "COMMA "
      | T_LBRAC -> print_string "LBRAC "
      | T_RBRAC -> print_string "RBRAC "
      | T_EOF -> print_string "EOF\n" 
  ) tokens
