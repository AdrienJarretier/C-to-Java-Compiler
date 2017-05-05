(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list}


exception UndefinedVar of string;;
exception UndefinedFun of string;;
exception TypeError of tp * tp;;
exception NumberOfParametersNotMatching;;


(* - : Typing.environment -> int Lang.expr -> Lang.tp Lang.expr = <fun> *)
let rec tp_expr env = function
    (Const (_, v) : int expr) -> let constType = function
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

  | BinOp (_, op, exp1, exp2) -> let tp1 = tp_of_expr(tp_expr env exp1)
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

  | IfThenElse (_, e1, e2, e3) -> let e1Typed = tp_expr env e1
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

  | CallE (_, fname, exprList) -> let rec funType = function
                (e::eList, param::pFunDecl) ->  let eTyped = (tp_expr env e) in
                                                let tpExpr = tp_of_expr eTyped and tpParam = tp_of_vardecl param in

                                              if tpExpr = tpParam
                                                then eTyped::funType(eList, pFunDecl)
                                              else
                                              raise (TypeError (tpExpr, tpParam))
              | ([], []) -> []
              | (_, _) -> raise NumberOfParametersNotMatching
                in
              let rec searchFunDecl = function
                (Fundecl (t, fn, pds))::funDecls ->
                            if fn = fname
                            then
                              CallE (t, fn, funType (exprList, pds))
                            else
                              searchFunDecl funDecls
              | _ -> raise (UndefinedFun fname)
                in
      searchFunDecl env.funbind
;;

let rec tp_stmt env = function

    Skip -> Skip

  | Assign (_, Var(binding, vname), exp) -> let rec varType varName = function
                    (name, varT)::localvars ->
                        if name = varName
                        then
                          varT
                        else
                          (varType varName localvars)
                    | _ -> raise (UndefinedVar varName)
                  in
                let typedExpr = (tp_expr env exp) in
            if tp_of_expr(typedExpr) = (varType vname env.localvar)
            then
              Assign(VoidT, Var(binding, vname), typedExpr)
            else
              raise (TypeError (tp_of_expr typedExpr, tp_of_expr typedExpr))

| Seq (stmt1 , stmt2) -> Seq((tp_stmt env stmt1), (tp_stmt env stmt2))

| Cond (exp, stmt1, stmt2) -> let typedExpr = (tp_expr env exp) in
                            if tp_of_expr(typedExpr)=BoolT
                            then
                              Cond(typedExpr, (tp_stmt env stmt1), (tp_stmt env stmt2))
                            else
                              raise (TypeError (tp_of_expr typedExpr, BoolT))

| While (exp, stmt) -> let typedExpr = (tp_expr env exp) in
                    if tp_of_expr(typedExpr) = BoolT
                    then
                      While(typedExpr, (tp_stmt env stmt))
                    else
                      raise (TypeError (tp_of_expr typedExpr, BoolT))

| CallC (fname, exprList) ->
                    let rec searchFunDecl = function
                      (Fundecl (t, fn, pds))::funDecls ->
                              if fn = fname
                              then
                                  let rec tpListe = function
                                    (exp::liste, param::paramList) ->
                                      let typedExpr = tp_expr env exp in
                                        if tp_of_expr(typedExpr) = tp_of_vardecl(param)
                                        then
                                          (typedExpr)::(tpListe(liste,paramList))
                                        else
                                          raise (TypeError (tp_of_expr(typedExpr), tp_of_vardecl(param)))
                                    | ([], []) -> []
                                    | (_, _) -> raise NumberOfParametersNotMatching
                                  in
                                tpListe(exprList,pds)
                              else
                                searchFunDecl funDecls
                      | _ -> raise (UndefinedFun fname)
                      in
                  CallC(fname, searchFunDecl env.funbind)

| Return (exp) -> let typedExpr = tp_expr env exp in
                if tp_of_expr(typedExpr) = env.returntp
                then
                  Return (typedExpr)
                else
                  raise (TypeError (tp_of_expr(typedExpr), env.returntp))
;;

(* *********************** TESTS ***********************

#use "use.ml";;
open Typing;;
open Lang;;
open Analyses;;

tp_expr;;


let env = {localvar = [("k", IntT); ("n", IntT)]; globalvar = [];
returntp = VoidT; funbind = [Fundecl(IntT , "f", [Vardecl(IntT , "n"); Vardecl(BoolT , "b")])]};;


(* n - 2 *)
let exprNminus2 = BinOp (0,
BArith BAsub ,
VarE (0, Var (Local , "n")),
Const (0, IntV 2));;


(* n = (k + 1) *)
let nEqualKPlus1 =
BinOp (0, BCompar BCeq ,
  VarE (0, Var (Local , "n")),
  BinOp (0, BArith BAadd,
    VarE (0, Var (Local , "k")),
    Const (0, IntV 1)
  )
);;



(* if n = (k + 1)
    then n - 2
    else 2
*)
let ifThenElseExpr =
IfThenElse (0, nEqualKPlus1, exprNminus2, Const (0, IntV 2));;




(* ********** Tests raising Exceptions ********** *)


(* n && 2 *)
let exprTypeError = BinOp (0,
BLogic BLand ,
VarE (0, Var (Local , "n")),
Const (0, IntV 2));;

(* n - false *)
let exprTypeError2 = BinOp (0,
BArith BAsub ,
VarE (0, Var (Local , "n")),
Const (0, BoolV false));;


(* if 2           => int
    then n - 2
    else 2
*)
let ifThenElseExprTypeError =
IfThenElse (0, Const (0, IntV 2), exprNminus2, Const (0, IntV 2));;


(* if n = (k + 1)
    then n - 2          => int
    else n = (k + 1)    => bool
*)
let ifThenElseExprTypeError2 =
IfThenElse (0, nEqualKPlus1, exprNminus2, nEqualKPlus1);;


(* ********** / Tests raising Exceptions ********** *)






(* ********** Partial application of tp_expr : ********** *)

let exprInEnv = tp_expr env;;


(* ********** using Function exprInEnv ********** *)




exprInEnv (Const (0, IntV 2));;
exprInEnv (VarE (0, Var (Local , "n")));;

exprInEnv exprNminus2;;
exprInEnv nEqualKPlus1;;
exprInEnv ifThenElseExpr;;
exprInEnv (CallE(0, "f", [Const (0, IntV 3); Const (0, BoolV true)]));;



exprInEnv (VarE (0, Var (Local , "notInEnvVar")));;

exprInEnv exprTypeError;;
exprInEnv exprTypeError2;;
exprInEnv ifThenElseExprTypeError;;
exprInEnv ifThenElseExprTypeError2;;
exprInEnv (CallE(0, "f", [Const (0, IntV 3); Const (0, BoolV true); Const (0, BoolV true)]));;
exprInEnv (CallE(0, "f", [Const (0, BoolV true)]));;
exprInEnv (CallE(0, "f", [Const (0, BoolV true); Const (0, BoolV true)]));;




*********************** / TESTS *********************** *)


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

