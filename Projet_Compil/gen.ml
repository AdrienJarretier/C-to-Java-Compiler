(* Compilation functions *)

open Lang
open Analyses
open Instrs

(* *********************** TESTS ***********************

#use "use.ml";;
open Gen;;
open Lang;;

gen_expr;;

let exprXminusYplus2 =
BinOp (IntT, BArith BAsub ,
  VarE (IntT, Var (Local , "x")),
  BinOp (IntT, BArith BAadd,
    VarE (IntT, Var (Local , "y")),
    Const (IntT, IntV 2)
  )
);;

gen_expr [("x", IntT); ("y", IntT)] exprXminusYplus2;;

 *)


(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)

let rec position = function
    (e, t::l) -> if t = e then 0 else 1+position(e, l)
  | _ -> failwith "element not in list";;


(*
- : (Lang.vname * Lang.tp) list -> Lang.tp Lang.expr -> Instrs.instr list = <fun>
*)
let rec gen_expr varList = function
    Const (t, v) -> [Loadc (t, v)]
  | VarE (t,Var (_, name)) -> [Loadv (t, position((name, t), varList))]
  | BinOp (tp, op, e1, e2) -> (gen_expr varList e1)@(gen_expr varList e2)@[Bininst (tp, op)];;



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) =
  JVMProg ([],
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])

