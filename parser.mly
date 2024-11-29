
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token FIX
%token IN
%token AS
%token CONCAT
%token BOOL
%token NAT
%token STRING
%token QUIT
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token LIST

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LARROW
%token RARROW
%token PIPE
%token CASE
%token OF
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token DOUBLEARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s : 
    IDT EQ ty EOF
      { TBind ($1, $3) }
    |IDV EQ term EOF
      { Bind ($1, $3) }
    |term EOF
      { Eval $1 }
    | QUIT EOF
      { Quit }


term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }


appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) }
  | FIX pathTerm
      { TmFix $2 }
  | appTerm pathTerm
      { TmApp ($1, $2) }


pathTerm :
	atomicTerm
      { $1 }
  | pathTerm DOT IDV
      { TmProj ($1, $3) }
  | pathTerm DOT INTV
      { TmProj ($1, string_of_int $3) }


atomicTerm :
  LPAREN term RPAREN
      { $2 }
	| LBRACE tupleTerm RBRACE
			{ TmTuple $2 }
	| LBRACE recordTerm RBRACE
			{ TmRecord $2 }
	| LARROW IDV EQ term RARROW AS ty
			{ TmTag ($2, $4, $7) }		
	| CASE term OF casesTerm
			{ TmCase ($2, $4) }

  | NIL LBRACKET ty RBRACKET
      { TmNil $3 }
  | CONS LBRACKET ty RBRACKET atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
  | ISNIL LBRACKET ty RBRACKET atomicTerm
      { TmIsNil ($3, $5) }
  | HEAD LBRACKET ty RBRACKET atomicTerm
      { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET atomicTerm
      { TmTail ($3, $5) }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
        { TmString $1 }


tupleTerm :
    term
      { [$1] }
  | term COMMA tupleTerm
      { $1 :: $3 }

recordTerm:
		{ [] }
	| nonEmptyRecordTerm
		{ $1 }

nonEmptyRecordTerm:
	IDV EQ term
      { [($1, $3)] }
  | IDV EQ term COMMA nonEmptyRecordTerm
      { ($1, $3) :: $5 }

casesTerm:
	LARROW STRINGV EQ IDV RARROW DOUBLEARROW term
			{ [($2, $4, $7)] }
	| LARROW STRINGV EQ IDV RARROW DOUBLEARROW term PIPE casesTerm
			{ ($2, $4, $7) :: $9 }



ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }


atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
	| IDT
			{ TyVar $1 }
  | LBRACE tupleTY RBRACE
      { TyTuple $2 }
  | LBRACE recordTY RBRACE
	  { TyRecord $2 }
	| LARROW nonEmptyRecordTY RARROW
		{ TyVariant $2 }
    | LIST LBRACKET ty RBRACKET
      { TyList $3 }



tupleTY :
		ty 
			{ [$1] }
  | ty COMMA tupleTY 
			{ $1 :: $3 }   

recordTY :
		{ [] }
	| nonEmptyRecordTY
		{ $1 }

nonEmptyRecordTY :
	IDV COLON ty
			{ [($1, $3)] }
	| IDV COLON ty COMMA nonEmptyRecordTY
			{ ($1, $3) :: $5 }

      
      