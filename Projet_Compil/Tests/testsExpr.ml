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
(*            in wrapper :    9            3            6           12                   *)
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
let a =
BinOp (0, BArith BAsub ,
  VarE (0, Var (Local , "n")),
  Const (0, IntV 2));;


(* x - (y + 2) *)
let b =
BinOp (0, BArith BAsub ,
  VarE (0, Var (Local , "x")),
  BinOp (0, BArith BAadd,
    VarE (0, Var (Local , "y")),
    Const (0, IntV 2)
  )
);;

(* a * b *)
let aTimesb =
BinOp (0, BArith BAmul ,
  a,
  b
);;


(* ************************* Partial Applications ************************* *)
(**)
(**) let typeInEnv = tp_expr env;;
(**) let genWIthEnvVars = gen_expr env.localvar;;
(**) let writetestsExprj = writeInFile "Tests/TestsExpr.j";;
(**)
(* ************************* Partial Applications ************************* *)

(* list of instr list
    each element contains the list of instruction for an expression
*)
let instList = [genWIthEnvVars (typeInEnv exprNmoins2)];;
let instList = instList@[genWIthEnvVars (typeInEnv exprXminusYplus2)];;
let instList = instList@[genWIthEnvVars (typeInEnv aTimesb)];;




(* to write the maximum number of arguments just ot fill the registers *)
let iParamsString = (let rec iStringArgument = function
                      0 -> ""
                    | n -> "I"^(iStringArgument (n-1))
                  in
iStringArgument (length env.localvar) );;

let stackLimit = (let rec maxLength = function
                      i::l -> let m = (length i) and prevMax = (maxLength l) in
                              if m > prevMax then m else prevMax
                    | _ -> 0
                  in
maxLength instList );;

let outString = ".class TestsExpr
.super java/lang/Object

.method static test("^iParamsString^")I
  .limit stack "^string_of_int(stackLimit)^"
  .limit locals "^string_of_int(length env.localvar)^"
"
  ;;


(* writes a method in TestsExpr.j for each element of instList *)
let outString = outString^
(let rec writeMethods = function
  i::l -> (pr_instrs 0 (i))
  ^ (writeMethods l)
  | _ -> ""
in
  writeMethods instList);;


let outString = outString^"
    ireturn
.end method
";;

writetestsExprj (outString);;


print_string("generated file "^"Tests/TestsExpr.j");;
