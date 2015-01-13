// Michael R. Hansen 03-01-2014
module AST
open System

type Exp = | Int of int 
           | Bool of bool 
           | String of string 
           | Var of string 
           | ContOf of Exp 
           | Apply of string * List<Exp>    
           | Arr of string * Exp
           | Field of string * string
           | Const of string

and  Stm = | Asg of Exp * Exp
           | AsgArr of string * Exp * Exp
           | PrintLn of Exp
           | Seq of List<Stm>
           | While of Exp * Stm
           | Block of List<Dec> * Stm
           | Call of string * List<Exp>
           | IfThen of Exp * Stm
           | IfElse of Exp * Stm * Stm
           | Ret of Exp

and Dec  = | VarDec of string * Exp
           | VarArr of string * Exp * Exp
           | Procedure of string * List<Exp> * Stm
           | RecProcedure of string * List<Exp> * Stm