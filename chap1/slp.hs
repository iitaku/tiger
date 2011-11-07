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
type Env = Map String Int

interp_stmt :: (Env, Stmt) -> (Env, IO ())
interp_stmt (env, CompoundStmt stmt1 stmt2) = (interp_stmt (interp_stmt (env, stmt1), stmt2), 
interp_stmt (env, AssignStmt id expr) = (case Data.Map.lookup id env of 
                                             Nothing  -> insert id expr env
                                             Just _   -> insert id expr $ delete id env, return ()) --interp_stmt (env, PrintStmt exprs) = env

interp_expr :: (Env, Expr) -> (Int, Env, IO ())
interp_expr (env, IdExpr id) = case Data.Map.lookup id env of
                                   Just x -> x
interp_expr (env, NumExpr int) = int
interp_expr (env, OpExpr expr1 op expr2) = 
    case op of
        Plus -> interp_expr (env, expr1) + interp_expr(env, expr2)
        Minus -> interp_expr (env, expr1) - interp_expr(env, expr2)
        Times -> interp_expr (env, expr1) * interp_expr(env, expr2)
        Div -> interp_expr (env, expr1) `div` interp_expr(env, expr2)
interp_expr (env, EseqExpr stmt expr) =                                    

interp :: Stmt -> IO ()
interp prog = snd interp_stmt (Data.Map.empty, prog)
