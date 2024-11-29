open Format
open Lambda

(* Print types based on the parser rules *)
let rec print_type = function
	TyArr (ty1, ty2) ->
			open_box 1;
			print_atomic_type ty1;
			print_string " -> ";
			print_space ();
			print_type ty2;
			close_box ()

	| ty -> print_atomic_type ty

(* Print atomic types *)
and print_atomic_type = function
	TyBool ->
			open_box 1;
			print_string "Bool";
			close_box ()
	| TyNat ->
			open_box 1;
			print_string "Nat";
			close_box ()
	| TyString ->
			open_box 1;
			print_string "String";
			close_box ()
	| TyTuple ts ->
			open_box 1;
			print_string "{";
			print_tuple ts;
			print_string "}";
			close_box ()
	| TyRecord ts ->
			open_box 1;
			print_string "{";
			print_record ts;
			print_string "}";
			close_box ()
	| TyVariant ts ->
			open_box 1;
			print_string "<";
			print_record ts;
			print_string ">";
			close_box ()
	| TyList ty ->
			open_box 1;
			print_string "List[";
			print_type ty;
			print_string "]";
			close_box ()

	| TyVar s ->
			open_box 1;
			print_string s;
			close_box ()
	| ty -> 
			open_box 1;
			print_string "(";
			print_type ty;
			print_string ")";
			close_box ()

and print_tuple = function
	[] -> ()
	| [x] -> print_type x
	| x :: xs ->
			print_type x;
			print_string ", ";
			print_space ();
			print_tuple xs

and print_record = function
	[] -> ()
	| [(s, term)] -> 
			print_string s; 
			print_string " : "; 
			print_type term

	| (s, term) :: t ->
			print_string s;
			print_string " : ";
			print_type term;
			print_string ", ";
			print_space ();
			print_record t

(* Print terms based on parser rules *)
let rec print_term = function

	TmIf (t1, t2, t3) ->
			open_box 1;
			print_string "if ";
			print_term t1;
			print_space ();
			print_string "then ";
			print_space ();
			print_term t2;
			print_space ();
			print_string "else ";
			print_space ();
			print_term t3;
			close_box ()

	| TmAbs (v, ty, t) ->
			open_box 1;
			print_string "lambda ";
			print_string v;
			print_string " : ";
			print_type ty;
			print_string ". ";
			print_space ();
			print_term t;
			close_box ()

	| TmLetIn (v, TmFix (TmAbs (v', ty', t')), t2) ->
			open_box 1;
			print_string "let rec ";
			print_string v;
			print_string " : ";
			print_type ty';
			print_string " = ";
			print_space ();
			print_term (TmFix (TmAbs (v', ty', t')));
			print_string " in ";
			print_space ();
			print_term t2;
			close_box ()

	| TmLetIn (v, t1, t2) ->
			open_box 1;
			print_string "let ";
			print_string v;
			print_string " = ";
			print_space ();
			print_term t1;
			print_string " in ";
			print_space ();
			print_term t2;
			close_box ()

	| term -> print_app_term term

(* Print appterms *)
and print_app_term = function

	TmSucc t ->
			open_box 1;
			let rec f n t' = match t' with
				TmZero -> print_string (string_of_int n)
				| TmSucc s -> f (n+1) s
				| _  -> print_string "succ "; 
								print_atomic_term t
			in f 1 t;
			close_box ()
	
	| TmPred t ->
			open_box 1;
			print_string "pred ";
			print_space ();
			print_path_term t;
			close_box ()

	| TmIsZero t ->
			open_box 1;
			print_string "iszero ";
			print_space ();
			print_path_term t;
			close_box ()

	| TmConcat (t1, t2) ->  
			open_box 1;
			print_string "concat ";
			print_space ();
			print_path_term t1;
			print_string " ";
			print_space ();
			print_path_term t2;
			close_box ()

	| TmFix t ->
			open_box 1;
			print_string "fix ";
			print_space ();
			print_path_term t;
			close_box ()

	| TmApp (t1, t2) ->
			open_box 1;
			print_app_term t1;
			print_string " ";
			print_space ();
			print_path_term t2;
			close_box ()

	| term -> print_path_term term

(* Print path terms *)
and print_path_term = function

	TmProj (t, s) ->
			open_box 1;
			print_path_term t;
			print_string ".";
			print_string s;
			close_box ()

	| term -> print_atomic_term term

(* Print atomic terms *)
and print_atomic_term = function
	
	TmTrue ->
			open_box 1;
			print_string "true";
			close_box ()
	
	| TmFalse ->
			open_box 1;
			print_string "false";
			close_box ()

	| TmVar v ->
			open_box 1;
			print_string v;
			close_box ()
	
	| TmZero ->
			open_box 1;
			print_string "0";
			close_box ()

	| TmString s ->
			open_box 1;
			print_string "\"";
			print_string s;
			print_string "\"";
			close_box ()
	
	| TmTuple ts ->
			open_box 1;
			print_string "{";
			print_tuple ts;
			print_string "}";
			close_box ()
	
	| TmRecord ts ->
			open_box 1;
			print_string "{";
			print_record ts;
			print_string "}";
			close_box ()

	| TmTag (s, t, ty) ->
			open_box 1;
			print_string "<";
			print_string s;
			print_string " = ";
			print_space ();
			print_term t;
			print_string ">";
			print_string " as ";
			print_type ty;
			close_box ()
	
	| TmCase (t, ts) ->
			open_box 1;
			print_string "case ";
			print_term t;
			print_string " of ";
			print_space ();
			print_cases ts;
			close_box ()
	| TmNil ty ->
			open_box 1;
			print_string "nil[";
			print_type ty;
			print_string "]";
			close_box ()

	| TmCons (ty, t1, t2) ->
        open_box 1;
        print_string "cons[";
        print_type ty;
        print_string "] ";
        print_app_term t1;
        print_string " ";
        let aux = function
            TmNil _ -> print_atomic_term t2
            | _ -> print_string"(";
                    print_atomic_term t2;
                    print_string ")"
        in aux t2;
        close_box ()

	| TmIsNil (ty, t) ->
			open_box 1;
			print_string "isnil[";
			print_type ty;
			print_string "] ";
			print_space ();
			print_term t;
			close_box ()
	| TmHead (ty, t) ->
			open_box 1;
			print_string "head[";
			print_type ty;
			print_string "] ";
			print_space ();
			print_term t;
			close_box ()
	| TmTail (ty, t) ->
			open_box 1;
			print_string "tail[";
			print_type ty;
			print_string "] ";
			print_space ();
			print_term t;
			close_box ()
	
	| term -> 
			open_box 1;
			print_string "(";
			print_term term;
			print_string ")";
			close_box ()

and print_tuple = function
	[] -> ()
	| [x] -> print_term x
	| x :: xs ->
			print_term x;
			print_string ", ";
			print_space ();
			print_tuple xs

and print_record = function
	[] -> ()
	| [(s, t)] -> print_string s; print_string " = "; print_term t
	| (s, t) :: xs ->
			print_string s;
			print_string " = ";
			print_term t;
			print_string ", ";
			print_space ();
			print_record xs

and print_cases = function
	[] -> ()
	| [(name, v, t)] -> 
			print_string "<";
			print_string name;
			print_string " = ";
			print_string v;
			print_string "> => ";
			print_space ();
			print_term t
	| (name, v, t) :: xs ->
			print_string "<";
			print_string name;
			print_string " = ";
			print_string v;
			print_string "> => ";
			print_space ();
			print_term t;
			print_string " | ";
			print_space ();
			print_cases xs

(* Entry points for printing *)
let print_eval tm ty =

	open_box 1;
	print_string "- : ";
	print_type ty;
	print_string " = ";
	print_space ();
	print_term tm;
	close_box ();
	force_newline ();
	print_flush ()

let print_bind s tm ty =

	open_box 1;
	print_string s;
	print_string " : ";
	print_type ty;
	print_string " = ";
	print_space ();
	print_term tm;
	close_box ();
	force_newline ();
	print_flush ()

let print_tbind s ty =

	open_box 1;
	print_string "type ";
	print_string s;
	print_string " = ";
	print_space ();
	print_type ty;
	close_box ();
	force_newline ();
	print_flush ()