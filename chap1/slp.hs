import Data.Map

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

-- count max argument of PrintStmt
maxarg_stmt :: (Int, Stmt) -> Int
maxarg_stmt (n, PrintStmt es) | n < (length es) = length es
                              | otherwise       = n
maxarg_stmt (n, CompoundStmt s1 s2) | sn1 < sn2 = sn2 
                                    | otherwise = sn1
                                    where sn1 = maxarg_stmt (n, s1)
                                          sn2 = maxarg_stmt (n, s2)
maxarg_stmt (n, AssignStmt i e) = maxarg_expr (n, e)

maxarg_expr :: (Int, Expr) -> Int
maxarg_expr (n, EseqExpr s e) | sn < en   = en
                              | otherwise = sn
                              where sn = maxarg_stmt (n, s)
                                    en = maxarg_expr (n, e)
maxarg_expr (n, OpExpr e1 op e2) | en1 < en2 = en2
                                 | otherwise = en1
                                 where en1 = maxarg_expr (n, e1)
                                       en2 = maxarg_expr (n, e2)
maxarg_expr (n, _) = n

maxarg :: Stmt -> Int
maxarg (CompoundStmt s1 s2) = maxarg_stmt (maxarg_stmt (0, s1), s2)
maxarg (AssignStmt i e)     = maxarg_expr (0, e)
maxarg (PrintStmt es)       = length es

-- interpreter of Stmt
type Env = [(String, Int)]

assign :: Env -> String -> Int -> Env
assign e s i | = 


interp_stmt :: (Env, Stmt) -> Env
interp_stmt (env, CompoundStmt s1 s2) = interp_stmt (interp_stmt (env, s1), s2)
interp_stmt (env, AssignStmt i e) = env

interp_expr :: (Env, Expr) -> Int
interp_expr IdExpr Id = 

interp :: Stmt -> ()
interp prog = ()