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
interp_stmt (env0, CompoundStmt stmt1 stmt2) = let (env1, io1) = interp_stmt (env0, stmt1)
                                                   (env2, io2) = interp_stmt (env1, stmt2)
                                               in (env2, io1 >>= (\ _ -> io2))
interp_stmt (env, AssignStmt id expr) = let (val1, env1, io1) = interp_expr (env, expr)
                                        in (case Data.Map.lookup id env of 
                                                Nothing  -> insert id val1 env1
                                                Just _   -> insert id val1 $ delete id env1, io1)

interp_stmt (env, PrintStmt (expr:[])) = let (val, env1, io1) =  interp_expr (env, expr)
                                         in (env1, io1 >>= (\ _ -> print $ show $ val))
interp_stmt (env, PrintStmt (expr:exprs)) = let (val, env1, io1) =  interp_expr (env, expr)
                                                (env2, io2) = interp_stmt (env1, PrintStmt exprs)
                                            in (env2, io1 >>= (\ _ -> print $ show $ val) >>= (\ _ -> io2))

interp_expr :: (Env, Expr) -> (Int, Env, IO ())
interp_expr (env, IdExpr id) = (case Data.Map.lookup id env of
                                     Just x -> x
                                     Nothing -> 0, env, return ())
interp_expr (env, NumExpr val) = (val, env, return ())
interp_expr (env0, OpExpr expr0 op expr1) = 
    let (val1, env1, io1) = interp_expr (env0, expr0)
        (val2, env2, io2) = interp_expr (env1, expr1)
    in (case op of
             Plus  -> val1   +   val2
             Minus -> val1   -   val2
             Times -> val1   *   val2
             Div   -> val1 `div` val2, env2, io1 >>= (\ _ -> io2))
interp_expr (env, EseqExpr stmt expr) = let (env1, io1) = interp_stmt (env, stmt)
                                            (val, env2, io2) = interp_expr (env1, expr)
                                        in (val, env2, io1 >>= (\ _ -> io2))

interp :: Stmt -> IO ()
interp prog = snd $ interp_stmt (Data.Map.empty, prog)
