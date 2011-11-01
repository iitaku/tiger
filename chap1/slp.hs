type Id = String

data Binop = Plus | Minus | Times | Div deriving Show

data Stmt = CompoundStmt Stmt Stmt | AssignStmt Id Expr | PrintStmt [Expr] deriving Show

data Expr = IdExpr Id | NumExpr Int | OpExpr Expr Binop Expr | EseqExpr Stmt Expr deriving Show

prog :: Stmt
prog = CompoundStmt (AssignStmt "a" (OpExpr (NumExpr 5) Plus (NumExpr 3))) 
                    (CompoundStmt (AssignStmt "b" 
                                              (EseqExpr (PrintStmt [IdExpr "a", OpExpr (IdExpr "a") Minus (NumExpr 1)]) 
                                                        (OpExpr (NumExpr 10) Times (IdExpr "a"))))
                                  (PrintStmt [IdExpr "b"]))

maxarg_ :: (Int, Stmt) -> Int

maxarg :: Stmt -> Int
maxarg (PrintStmt xs)       = length xs
maxarg (CompoundStmt s1 s2) = maxarg s1 + maxarg s2
maxarg (AssignStmt i e)     = maxarg e
maxarg (OpExpr e1 b e2)     = maxarg e1 + maxarg e2
maxarg (EseqExpr s e)       = maxarg s + maxarg e
maxarg _                    = 0 
