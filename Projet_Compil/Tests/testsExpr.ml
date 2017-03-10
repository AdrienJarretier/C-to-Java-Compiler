
(*

Write this in Ocaml interpreter :

#use "use.ml";;
open Lang;;
#use "Tests/testsExpr.ml";;

*)

open Typing
open Gen
open Print_instr


(* ************************* Env for tests ************************* *)
(**)
(**) let env = {localvar = [("k", IntT); ("n", IntT); ("x", IntT); ("y", IntT)]; globalvar = [];
(**) returntp = VoidT; funbind = [Fundecl(IntT , "f", [Vardecl(IntT , "n"); Vardecl(BoolT , "b")])]};;
(**)
(* ************************* Env for tests ************************* *)



(* write s to outfile *)
let writeInFile outfile s =
  let outf = open_out outfile in
 	output_string outf s ; flush outf
;;



(* n - 2 *)
let exprNmoins2 = BinOp (0,
BArith BAsub ,
VarE (0, Var (Local , "n")),
Const (0, IntV 2));;



(* partial applications *)
let typeInEnv = tp_expr env;;
let genWIthEnvVars = gen_expr env.localvar;;



writeInFile "Tests/testsExpr.j" (pr_instrs 0 (genWIthEnvVars (typeInEnv exprNmoins2)));;

print_string("generated file "^"Tests/testsExpr.j");;
