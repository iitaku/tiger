type pos = { line:int; column:int }
type symbol = { name:string; value:int }

type field = { name:symbol; escape:bool ref}

type var = SimpleVar    of symbol       * pos
         | FieldVar     of var * symbol * pos
         | SubscriptVar of var * expr   * pos

and expr = VarExpr     of var                                 * pos
         | NilExpr     of                                       pos
         | IntExpr     of int                                 * pos
         | StringExpr  of string                              * pos
         | OperandExpr of var * operand * var                 * pos
         | AssignExpr  of var * expr                          * pos
         | SeqExpr     of (expr * pos) list
         | RecordExpr  of symbol * (symbol * expr * pos) list * pos
         | CallExpr    of symbol * var array                  * pos 
         | ArrayExpr   of symbol * expr * expr                * pos
         | BranchExpr  of expr * expr * expr                  * pos
         | LoopExpr    of expr                                * pos
         | BreakExpr   of                                       pos
         | LetExpr     of (dec list) * expr                   * pos

and operand = PlusOp | MinusOp | TimesOp | DivideOp
            | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

and dec = TyDec  of {name:symbol; ty:ty; pos:pos}
        | VarDec of var * symbol * pos
        | FunDec of fundec list * pos

and ty = NameTy of symbol * pos
       | RecordTy of field list * pos
       | ArrayTy of symbol * pos

and fundec = { name:symbol; param:field list; result:(symbol * pos) option; body:expr; pos:pos }
