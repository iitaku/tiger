type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog2 = 
    CompoundStm(AssignStm("1", NumExp 1), PrintStm[IdExp "1"])

(* maxarg *)
fun maxarg_stm(n, PrintStm(es)) = if n < List.length(es) then List.length(es) else n
 |  maxarg_stm(n, CompoundStm(s1, s2)) = 
        let 
            val sn1 = maxarg_stm(n, s1)
            val sn2 = maxarg_stm(n, s2)
        in 
            if sn1 < sn2 then sn2 else sn1
        end
 |  maxarg_stm(n, AssignStm(i, e)) = maxarg_exp(n, e)
and 
    maxarg_exp(n, EseqExp(s, e)) = 
        let 
            val sn = maxarg_stm(n, s)
            val en = maxarg_exp(n, e)
        in
            if sn < en then en else sn
        end
 |  maxarg_exp(n, OpExp(e1, _, e2)) =
        let 
            val en1 = maxarg_exp(n, e1)
            val en2 = maxarg_exp(n, e2)
        in
            if en1 < en2 then en2 else en1
        end
 |  maxarg_exp(n, _) = n

fun maxargs(CompoundStm(s1, s2)) = maxarg_stm(maxarg_stm(0, s1), s2)
 |  maxargs(AssignStm(i, e))     = maxarg_exp(0, e)
 |  maxargs(PrintStm(es))        = List.length(es)

(* env *)
fun update(env, (key, value)) = (key, value) :: env

fun lookup(env, key) = 
        let
            val (hd_key, hd_value) = List.hd(env)
        in
            if EQUAL = String.compare(hd_key, key) then hd_value else lookup(List.tl(env), key)
        end

(* interpreter *)
fun interp_stm(env, CompoundStm(s0, s1)) =
        let 
            val new_env = interp_stm(env, s0);
        in
            interp_stm(new_env, s1)
        end
 |  interp_stm(env, AssignStm(key, e)) = 
        let 
            val (new_env, value) = interp_exp(env, e)
        in
            update(new_env, (key, value))
        end
 |  interp_stm(env, PrintStm(e::[])) = 
        let 
            val (new_env : (id * int) list, value : int) = interp_exp(env, e)
        in
            (print(Int.toString(value)^"\n"); new_env)
        end
 |  interp_stm(env, PrintStm(e::es)) = 
        let
            val (new_env : (id * int) list, value : int) = interp_exp(env, e)
        in
            (print(Int.toString(value)^"\n"); interp_stm(new_env, PrintStm(es)))
        end
and 
    interp_exp(env, IdExp key) = (env, lookup(env, key))
 |  interp_exp(env, NumExp value) = (env, value)
 |  interp_exp(env, OpExp(e1, ops, e2)) = 
        let
            val (env1 : (id * int) list, v1 : int) = interp_exp(env, e1)
            val (env2 : (id * int) list, v2 : int) = interp_exp(env1, e2)
        in
            (env2,
             case ops of Plus  => v1  +  v2
                       | Minus => v1  -  v2
                       | Times => v1  *  v2
                       | Div   => v1 div v2)
        end
 |  interp_exp(env, EseqExp(s, e)) = 
        let
            val new_env = interp_stm(env, s)
        in
            interp_exp(new_env, e)
        end

fun interp(p) = (interp_stm([] : (id * int) list, p); ());
