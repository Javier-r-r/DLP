
(* TYPE DEFINITIONS *)

type ty =
  TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyVar of string
  | TyList of ty
;;

type term =
  TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term list
  | TmProj of term * string
  | TmRecord of (string * term) list
  | TmTag of string * term * ty
  | TmCase of term * (string * string * term) list
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
  | TmCons of ty * term * term
  | TmNil of ty
;;

type command =
  Eval of term
  | Bind of string * term
  | TBind of string * ty
  | Quit
;;

type binding =
  TyBind of ty
  | TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addvbinding ctx s ty tm =
  (s, TyTmBind (ty, tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with
    TyBind ty -> ty
    | TyTmBind (ty, _) -> ty
;;

let getvbinding ctx s =
  match List.assoc s ctx with
    TyTmBind (_, tm) -> tm
    | _ -> raise Not_found
;;

(* TYPE MANAGEMENT (TYPING) *)


exception Type_error of string
;;

let rec subtypeof t1 t2 = match (t1, t2) with
  (TyRecord(l1), TyRecord(l2)) ->
    let check (x, ty) l =
      try 
        subtypeof ty (List.assoc x l)
      with Not_found -> false
    in let rec contains l1 l2 = 
      match l1 with
          [] -> true
        | h::t -> check h l2 && contains t l2
      in contains l1 l2
  | (TyArr(s1, s2), TyArr(t1, t2)) -> subtypeof s1 t1 && subtypeof s2 t2
  | _ -> t1 = t2
;;


let rec base_ty ctx ty = match ty with
  TyBool -> TyBool
  | TyNat -> TyNat
  | TyString -> TyString
  | TyArr (ty1, ty2) -> TyArr (base_ty ctx ty1, base_ty ctx ty2)
  | TyTuple tys -> TyTuple (List.map (base_ty ctx) tys)
  | TyRecord tys -> TyRecord (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVariant tys -> TyVariant (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVar s -> (try gettbinding ctx s with _ -> raise (Type_error ("no binding type for variable" ^s)))
  | TyList ty -> TyList (base_ty ctx ty)
;;


let rec typeof ctx tm = match tm with
    (* T-True *)
  TmTrue ->
    TyBool

    (* T-False *)
  | TmFalse ->
    TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
    if typeof ctx t1 = TyBool then
      let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
    else
      raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
    TyNat

    (* T-Succ *)
  | TmSucc t1 ->
    if typeof ctx t1 = TyNat then TyNat
    else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
    if typeof ctx t1 = TyNat then TyNat
    else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
    if typeof ctx t1 = TyNat then TyBool
    else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
    (try gettbinding ctx x with
      _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
    let ctx' = addtbinding ctx x tyT1 in
      let tyT1' = base_ty ctx tyT1 in
        let tyT2 = typeof ctx' t2 in
          TyArr (tyT1', tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
    let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
        (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if subtypeof tyT11 tyT2 then tyT12
            else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
    let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
        typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
    let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if subtypeof tyT11 tyT12 then tyT12
          else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))

    (* new rule for string *)
  | TmString _ ->
    TyString

    (* new rule for string *)  
  | TmConcat (t1, t2) ->
    if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
    else raise (Type_error "arguments of concat is not a string")

    (* T-Tuple *)
  | TmTuple t1 ->
    TyTuple (List.map (typeof ctx) t1)

    (* T-Proj *)
  | TmProj (t, index) ->
    (match typeof ctx t with
      TyTuple ty ->
        let i = int_of_string index in
          if i > 0 && i <= List.length ty then List.nth ty (i - 1)
          else raise (Type_error "index out of bounds")
      | TyRecord ty ->
        let rec aux = function
          (s, ty) :: t when s = index -> ty
          | _ :: t -> aux t
          | [] -> raise (Type_error "field not found")
        in aux ty
      | _ -> raise (Type_error " expected tuple or record"))
  
    (* T-Record *)
  | TmRecord t1 ->
    TyRecord (List.map (fun (s, t) -> (s, typeof ctx t)) t1)
  
    (* T-Variant *)
  | TmTag (s, t1, ty) ->
    let tyT1 = base_ty ctx ty in
      (match tyT1 with
        TyVariant ty' ->
          if List.mem_assoc s ty' then
            if List.assoc s ty' = typeof ctx t1 then tyT1
            else raise (Type_error "field does not match")
          else raise (Type_error "field not found")
        | _ -> raise (Type_error "expected variant type"))

    (* T-Case *)
  | TmCase (t, cases) ->
    (match typeof ctx t with
      TyVariant ty ->
        let rec aux = function
          (s, x, t) :: t' ->
            let ctx' = addtbinding ctx x (List.assoc s ty) in
              if typeof ctx' t = typeof ctx (TmVar x) then aux t'
              else raise (Type_error "case arms do not match")
          | [] -> List.assoc "result" ty
        in aux cases
      | _ -> raise (Type_error "expected variant type"))

    (* T-Nil *)
  | TmNil ty ->
    TyList ty

    (* T-Cons *)
  | TmCons (ty, t1, t2) ->
    let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
        (match tyT2 with
          TyList ty' ->
            if (subtypeof tyT1 ty) && (subtypeof tyT2 (TyList(ty))) then TyList(ty)
            else raise (Type_error "arguments of cons are not the same type")
          | _ -> raise (Type_error "second argument of cons is not a list"))

    
    (* T-IsNil *)
  | TmIsNil (ty, t) ->
    if typeof ctx t = TyList ty then TyBool
    else raise (Type_error "argument of isNil is not a list")

  (* T-Head *)
  | TmHead (ty, t) ->
      let tyT = typeof ctx t in
        (match tyT with
          TyList _ -> ty
          | _ -> raise (Type_error "argument of head is not a list")) 
      
    (* T-Tail *)
  | TmTail (ty, t) ->
    if typeof ctx t = TyList ty then TyList ty
    else raise (Type_error "argument of tail is not a list")
;;


(* TERMS MANAGEMENT (EVALUATION) *)



let rec ldif l1 l2 = match l1 with
  [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
  [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
  TmTrue ->
    []
  | TmFalse ->
    []
  | TmIf (t1, t2, t3) ->
    lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
    []
  | TmSucc t ->
    free_vars t
  | TmPred t ->
    free_vars t
  | TmIsZero t ->
    free_vars t
  | TmVar s ->
    [s]
  | TmAbs (s, _, t) ->
    ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
    lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
    free_vars t
  | TmString _ ->
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmTuple ts ->
    let rec aux = function
      h :: [] -> free_vars h
      | h :: t -> lunion (free_vars h) (aux t)
      | [] -> []
    in aux ts
  | TmProj (t, _) ->
    free_vars t
  | TmRecord ts ->
    let rec aux = function 
      (s, term) :: [] -> free_vars term
      | (s, term) :: t -> lunion (free_vars term) (aux t)
      | [] -> []
    in aux ts
  | TmTag (_, t, _) ->
    free_vars t
  | TmCase (t, cases) ->
    let rec aux = function
      (s, x, t) :: [] -> lunion (free_vars t) (ldif (free_vars (TmVar x)) [s])
      | (s, x, t) :: t' -> lunion (lunion (free_vars t) (ldif (free_vars (TmVar x)) [s])) (aux t')
      | [] -> []
    in lunion (free_vars t) (aux cases)
  | TmNil _ ->
    []
  | TmIsNil (_, t) ->
    free_vars t
  | TmHead (_, t) ->
    free_vars t
  | TmTail (_, t) ->
    free_vars t
  | TmCons (_, t1, t2) ->
    lunion (free_vars t1) (free_vars t2)  
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))

  | TmFix t ->
     TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple ts ->  
      TmTuple (List.map (subst x s) ts)
  | TmProj (t1, t2) ->
      TmProj (subst x s t1, t2)
  | TmRecord ts ->
      TmRecord (List.map (fun (name, term) -> (name, subst x s term)) ts)
  | TmTag (name, t, ty) ->
      TmTag (name, subst x s t, ty)
  | TmCase (t, cases) ->
      TmCase (subst x s t, List.map (fun (name, var, term) -> if var = x then (name, var, term) else (name, var, subst x s term)) cases)
      | TmNil ty ->
        TmNil ty
    | TmCons (ty, t1, t2) ->
        TmCons (ty, subst x s t1, subst x s t2)
    | TmIsNil (ty, t) ->
        TmIsNil (ty, subst x s t)
    | TmHead (ty, t) ->
        TmHead (ty, subst x s t)
    | TmTail (ty, t) ->
        TmTail (ty, subst x s t)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple t -> List.for_all isval t
  | TmRecord [] -> true
  | TmRecord t -> List.for_all (fun (_, t) -> isval t) t
  | TmTag (_, t, _) -> isval t
  | TmCase (t, cases) -> isval t && List.for_all (fun (_, _, t) -> isval t) cases
  | TmNil _ -> true
  | TmCons (_, t1, t2) -> isval t1 && isval t2
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)
  
    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

    (* new rule for string *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

    (* new rule for string *) 
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')

    (* new rule for string *) 
  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat (t1', t2)
      
  | TmVar s ->
      getvbinding ctx s

    (* E-Tuple*)
  | TmTuple ts ->
      let rec aux = function
          h :: t when isval h -> let t' = aux t in h::t'
        | h :: t -> let h' = eval1 ctx h in h'::t
        | [] -> raise NoRuleApplies
      in TmTuple (aux ts)

    (* E-ProjTuple *) 
  | TmProj (TmTuple t, s) ->
      let rec aux i = function
          h :: t when i = 1 -> h
        | h :: t -> aux (i-1) t
        | [] -> raise NoRuleApplies
      in aux (int_of_string s) t  

    (* E-Record *)
  | TmRecord ts ->
      let rec aux = function
          (s, term) :: t when isval term -> let t' = aux t in (s, term)::t'
        | (s, term) :: t -> let h' = eval1 ctx term in (s, h')::t
        | [] -> raise NoRuleApplies
      in TmRecord (aux ts)

    (* E-RecordProj *)
  | TmProj (TmRecord ts, s) ->
      let rec aux = function
          (s', term) :: t when s = s' -> term
        | _ :: t -> aux t
        | [] -> raise NoRuleApplies
      in aux ts

      (* E-Proj *)
  | TmProj (t, s) ->
      let t' = eval1 ctx t in
      TmProj (t', s)
  
    (* E-Tag *)
  | TmTag (name, t, ty) ->
      let t' = eval1 ctx t in
      TmTag (name, t', ty)

    (* E-Case *)
  | TmCase (TmTag (tag, v1, _), cases) when isval v1 ->
      let (_, var, t) = List.find (fun (_, tg, _) -> tg = tag) cases in subst var v1 t 

    (* E-Case *)
  | TmCase (t, cases) ->
      let t' = eval1 ctx t in
      TmCase (t', cases)
    
         (* E-Cons1 *)
  | TmCons (ty, t1, t2) when isval t1 ->
          let t2' = eval1 ctx t2 in
          TmCons (ty, t1, t2')
    
        (* E-Cons2 *)
  | TmCons (ty, t1, t2) ->
          let t1' = eval1 ctx t1 in
          TmCons (ty, t1', t2)
    
        (* E-IsNilNil *)
  | TmIsNil (_, TmNil(_)) ->
          TmTrue
    
          (* E-IsNilCons *)
  | TmIsNil (_, TmCons(_,_,_)) ->
          TmFalse
    
          (* E-IsNil *)
  | TmIsNil (ty, t) ->
          let t' = eval1 ctx t in
          TmIsNil (ty, t')
    
          (* E-HeadCons *)
  | TmHead (_, TmCons(_, t1, _)) when isval t1 ->
          t1
    
          (* E-Head *)
  | TmHead (ty, t) ->
          let t' = eval1 ctx t in
          TmHead (ty, t')
    
          (* E-TailCons *)
  | TmTail (_, TmCons(_, _, t2)) when isval t2 ->
          t2
    
          (* E-Tail *)
  | TmTail (ty, t) ->
          let t' = eval1 ctx t in
          TmTail (ty, t')    

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;; 

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;




