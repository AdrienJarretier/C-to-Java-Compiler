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


let exprTypeError = BinOp (0,
BLogic BLand ,
VarE (0, Var (Local , "n")),
Const (0, IntV 2));;

let exprTypeError2 = BinOp (0,
BArith BAsub ,
VarE (0, Var (Local , "n")),
Const (0, BoolV false));;

let nEgalKPlus1 = BinOp (0, BCompar BCeq , VarE (0, Var (Local , "n")),
BinOp (0, BArith BAadd , VarE (0, Var (Local , "k")),
Const (0, IntV 1)));;


let ifThenElseExprTypeError = IfThenElse (0, Const (0, IntV 2), exprNmoins2, Const (0, IntV 2));;
let ifThenElseExprTypeError2 = IfThenElse (0, nEgalKPlus1, exprNmoins2, nEgalKPlus1);;
let ifThenElseExpr = IfThenElse (0, nEgalKPlus1, exprNmoins2, Const (0, IntV 2));;


exception UndefinedVar of string;;
exception TypeError of tp * tp;;



let rec tp_expr env = function
	  Const (0, v) -> let constType = function
					  BoolV _ -> BoolT
					| IntV _ -> IntT
					| VoidV -> VoidT in
	  		Const (constType v, v)

	| VarE (0, Var(binding, vname)) -> let rec varType varName = function
									  (name, varT)::localvars ->
									  		if name = varName
			  								then
			  									varT
			  								else
			  									(varType varName localvars)
									  | _ -> raise (UndefinedVar varName) in
			VarE (varType vname env.localvar, Var(binding, vname))

	| BinOp (0, op, exp1, exp2) -> let tp1 = tp_of_expr(tp_expr env exp1)
									and tp2 = tp_of_expr(tp_expr env exp2) in
			if tp1 = tp2
			then
				let checkTypeWithOp = function
					  (BArith _, IntT) -> IntT
					| (BLogic _, BoolT) -> BoolT
					| (BCompar _, _) -> BoolT
					| (op, expType) -> raise (TypeError (expType, expType))
				in
				BinOp (checkTypeWithOp(op, tp1), op, tp_expr env exp1, tp_expr env exp2)
			else
				raise (TypeError (tp1,tp2))

	| IfThenElse (0, e1, e2, e3) -> let e1Typed = tp_expr env e1
									and e2Typed = tp_expr env e2
									and e3Typed = tp_expr env e3
									in
			let tp1=tp_of_expr(e1Typed) and tp2=tp_of_expr(e2Typed) and tp3=tp_of_expr(e3Typed) in
				if tp1 = BoolT
				then
					if tp2 = tp3
					then
						IfThenElse (tp2, e1Typed, e2Typed, e3Typed)
					else
						raise (TypeError (tp2, tp3))
				else
					raise (TypeError (tp1, tp1))
;;

(*
#use "use.ml";;
open Typing;;
open Lang;;
open Analyses;;

tp_expr;;

let exprInEnv = tp_expr env;;

exprInEnv (Const (0, IntV 2));;

exprInEnv (VarE (0, Var (Local , "n")));;
exprInEnv (VarE (0, Var (Local , "notInEnvVar")));;

exprInEnv exprNmoins2;;
exprInEnv exprTypeError;;
exprInEnv exprTypeError2;;

exprInEnv nEgalKPlus1;;

tp_of_expr(exprInEnv nEgalKPlus1) = IntT;;
tp_of_expr(exprInEnv nEgalKPlus1) = BoolT;;


exprInEnv ifThenElseExprTypeError;;
exprInEnv ifThenElseExprTypeError2;;
exprInEnv ifThenElseExpr;;

*)


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

