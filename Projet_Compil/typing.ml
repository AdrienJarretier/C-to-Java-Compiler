(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list}


let env = {localvar = [("k", IntT); ("n", IntT)]; globalvar = [];
returntp = VoidT; funbind = []};;


let exprNmoins2 = BinOp (0,
BArith BAsub ,
VarE (0, Var (Local , "n")),
Const (0, IntV 2));;


exception UndefinedVar of string;;



let rec tp_expr env = function
	  Const (_, v) -> let constType = function
					  BoolV _ -> BoolT
					| IntV _ -> IntT
					| VoidV -> VoidT in
	  		Const (constType v, v)

	| VarE (_, Var(binding, vname)) -> let rec varType varName = function
									  (name, varT)::localvars ->
									  		if name = varName
			  								then
			  									varT
			  								else
			  									(varType varName localvars)
									  | _ -> raise (UndefinedVar varName) in
			VarE (varType vname env.localvar, Var(binding, vname))
;;

(*
#use "use.ml";;
open Typing;;
open Lang;;
open Analyses;;

tp_expr env (Const (0, IntV 2));;

tp_expr env (VarE (0, Var (Local , "n")));;
tp_expr env (VarE (0, Var (Local , "notInEnvVar")));;

*)


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

