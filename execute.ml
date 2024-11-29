open Lambda
open PrettyPrinter


let execute ctx = function
  | Eval tm ->
    let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
        print_eval tm' tyTm;
      (* print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm'); *)
        ctx

  | Bind (s, tm) ->
    let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
        print_bind s tm' tyTm;
      (* print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm'); *)
        addvbinding ctx s tyTm tm'
  
  | TBind (s, ty) ->
    let bty = base_ty ctx ty in 
      print_tbind s bty;
      addtbinding ctx s bty

  | Quit -> 
    raise End_of_file
;;