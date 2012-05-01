type id = string

type pos = { line:int; column:int }

type type_alias = { new_type_id:id; old_type_id:id }
 
type field = { name_id:id; type_id:id; is_escaped:bool ref }

type ty_decl =   NameType of type_alias * pos
             | RecordType of field list * pos
             |  ArrayType of id         * pos

and var_decl = { var_name:id; right_value:expr; type_id:id option }

and fun_decl = { fun_name:id; param:field list;  body:expr; result:ty_decl option

and decl = TypeDecl of type_decl list * pos
         |  VarDecl of  var_decl list * pos
         | FuncDecl of  fun_decl list * pos

and var_ref = SimpleVarRef of id * pos
            | RecordVarRef of field * 

and expr =     VarExpr of var                                 * pos
         |     NilExpr of                                       pos
         |     IntExpr of int                                 * pos
         |  StringExpr of string                              * pos
         | OperandExpr of var * operand * var                 * pos
         |  AssignExpr of var * expr                          * pos
         |     SeqExpr of (expr * pos) list
         |  RecordExpr of symbol * (symbol * expr * pos) list * pos
         |    CallExpr of symbol * var array                  * pos 
         |   ArrayExpr of symbol * expr * expr                * pos
         |  BranchExpr of expr * expr * expr                  * pos
         |    LoopExpr of expr                                * pos
         |   BreakExpr of                                       pos
         |     LetExpr of (decl list) * expr                   * pos

and operand = PlusOp | MinusOp | TimesOp | DivideOp
            | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
