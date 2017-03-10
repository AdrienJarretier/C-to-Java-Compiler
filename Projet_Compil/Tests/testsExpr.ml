(*

Write this in Ocaml interpreter (run it from Projet_Compil dir) :

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

(* get length of list *)
let rec length = function
    t::l -> 1 + (length l)
  | _ -> 0
;;



(* n - 2 *)
let exprNmoins2 = BinOp (0,
BArith BAsub ,
VarE (0, Var (Local , "n")),
Const (0, IntV 2));;



(* ************************* Partial Applications ************************* *)
(**)
(**) let typeInEnv = tp_expr env;;
(**) let genWIthEnvVars = gen_expr env.localvar;;
(**) let writetestsExprj = writeInFile "Tests/TestsExpr.j";;
(**)
(* ************************* Partial Applications ************************* *)

let instructions = genWIthEnvVars (typeInEnv exprNmoins2);;

let outString = ".class TestsExpr
.super java/lang/Object

.method static test(II)I
  .limit stack "^string_of_int(length instructions)^"
  .limit locals "^string_of_int(length env.localvar);;

let outString = outString^(pr_instrs 0 (instructions));;

let outString = outString^"
	ireturn
.end method";;


writetestsExprj (outString);;


print_string("generated file "^"Tests/TestsExpr.j");;
