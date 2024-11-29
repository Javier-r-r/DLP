
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

val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val addvbinding : context -> string -> ty -> term -> context;;
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;


exception Type_error of string;;
val typeof : context -> term -> ty;;


exception NoRuleApplies;;
val eval : context -> term -> term;;
val base_ty : context -> ty -> ty;;
