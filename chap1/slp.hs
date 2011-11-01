type Id = String

data Binop = Plus | Minus | Times | Div deriving Show

data Stmt = CompoundStmt Stmt Stmt | AssignStmt Id Expr | PrintStmt [Expr] deriving Show

data Expr = IdExpr Id | NumExpr Integer | OpExpr Expr Binop Expr | EseqExpr Stmt Expr deriving Show

prog = CompoundStmt (AssignStmt "a" (OpExpr (NumExpr 5) Plus (NumExpr 3))) 
                    (CompoundStmt (AssignStmt "b" 
                                              (EseqExpr (PrintStmt [IdExpr "a", OpExpr (IdExpr "a") Minus (NumExpr 1)]) 
                                                        (OpExpr (NumExpr 10) Times (IdExpr "a"))))
                                  (PrintStmt [IdExpr "b"]))
