(* Interpreter for a simple WHILE-language. Michael R. Hansen 03-01-2014 *)
(* Based on a natural semantics of WHILE                                 *)

(* Remember to regenerate the parser and the lexer using the commands 
   in README.txt if you modified the parser and lexer                    *)

module Interpreter 

open System
open AST

type Location = int
type Value    = | IntVal of int 
                | BoolVal of bool 
                | StringVal of string 
                | Reference of Location 
                | Primitive of (List<Value> -> Value)
and Env       = Map<string,Value>


type Closure =  List<string> * Env * Stm

type Content = SimpVal of Value | Proc of Closure |  ArrayCnt of Value [];;

type Store  = Map<Location,Content>  
  
let closureOf(ps,st) env = (ps, env, st)

// nextLoc() generates the next available location
let nextLoc: unit -> int =  let n = ref 0
                            let f x = (n := !n+1; !n)
                            f

let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

// exp: Exp -> Env -> Store -> Value * Store 
let rec exp e (env:Env) (store:Store) = 
    match e with
    | Const c     -> match Map.find c env with
                     | Reference refl -> 
                         let loc = nextLoc()
                         let value = (Map.find refl store)
                         let store2 = Map.add loc value store
                         (Reference loc, store2)
                     | _                     -> failwith "errorYYY"
    | Var v       -> match Map.find v env with
                     | Reference loc as refl -> (refl,store)
                     | IntVal i              -> printfn "%s" (string i) ; failwith "errorXXX"
                     | _                     -> failwith "errorYYY"
    | ContOf er    -> match exp er env store with
                      | (Reference loc,store1) -> match Map.find loc store1 with 
                                                  | SimpVal res -> (res,store1)
                                                  | Proc p -> (Reference loc,store1)
                                                  | ArrayCnt a -> (Reference loc,store1)
                                                  | _           -> failwith "error"
                      | _                   -> failwith "error"

    | Apply(f,es) -> let (vals, store1) = expList es env store
                     match Map.find f env with 
                     | Primitive f   -> (f vals, store1) 
                     | Reference l   -> 
                        match Map.find l store with
                        | Proc(sl, env2, func) -> 
                            let env3 = Map.ofList (List.zip sl vals)
                            match stm func (join env2 env3) store1 with
                            | (Some(a), b) -> (a,b)
                            | (None, b) ->failwith "error"
                        | _ -> failwith "error"
                     | _              -> failwith "type error"          
                                                               
    | Int i       -> (IntVal i, store)
    | Bool b      -> (BoolVal b,store)
    | String s    -> (StringVal s,store)
    | Arr(name, index) ->
        let i = match exp index env store with | (IntVal(v), _) -> v | _ -> failwith "error"
        let loc = 
            match Map.find name env with 
            | Reference(l) -> l 
            | _ -> failwith "error"
        match Map.find loc store with
        | ArrayCnt(a) -> (a.[i], store)
        | _ -> failwith "error"
    | Field(name, field) ->
        let loc = 
            match Map.find name env with 
            | Reference(l) -> l 
            | _ -> failwith "error"
        match Map.find loc store with
        | ArrayCnt(a) -> 
            if field="length" then (IntVal a.Length, store) else failwith "error"
        | _ -> failwith "error"

and expList es env store = 
    match es with 
    | []       -> ([],store)
    | e::erest -> let (res1, store1) = exp e env store
                  let (ress, store2) = expList erest env store1
                  (res1::ress, store2)  

// stm: Stm -> Env -> Store -> option<Value> * Store
and stm st (env:Env) (store:Store) = 
    match st with 
    | Asg(el,e) -> let (res,store1) = exp e env store
                   let (resl, store2) = exp el env store1
                   match resl with 
                   | Reference loc -> 
                       match res with
                       | Reference r ->
                           let value = Map.find r store2
                           (None, Map.add loc value store2)
                       | _ -> (None, Map.add loc (SimpVal res) store2) 
                   | _                               -> failwith "type error"
    
    | AsgArr(name, index, value) ->
        let i = match exp index env store with | (IntVal(v), _) -> v | _ -> failwith "error"
        let loc = 
            match Map.find name env with 
            | Reference(l) -> l 
            | _ -> failwith "error"
        match Map.find loc store with
        | ArrayCnt(a) ->
            let (res, store2) = exp value env store
            Array.set a i res
            (None, store2)
        | _ -> failwith "error"        
         
    | PrintLn e -> match exp e env store with
                   | (StringVal s,store1) -> (printfn "%s" s; (None,store1))
                   | _                    -> failwith "error"                  
                                                                 
                                           
    | Seq []        -> (None,store)
    | Seq (st::sts) -> match stm st env store with 
                       | (None, store1)   -> stm (Seq sts) env store1
                       | result       -> result
    | Ret(e)        -> match exp e env store with | (a,b) -> (Some(a), b)
    | While(e,st1)  -> let (res, store1) = exp e env store
                       match res with 
                       | BoolVal true  -> match stm st1 env store1 with
                                          | (None, store2) -> stm st env store2
                                          | result     -> result
                       | BoolVal false -> (None, store1)
                       | _             -> failwith "type error"                     
 
    | Block(ds,st1) -> let (env1,store1) = decList ds env store 
                       stm st1 env1 store1
    | Call(name, values) ->
        let loc = 
            match Map.find name env with 
            | Reference(l) -> l 
            | _ -> failwith "error"
        match Map.find loc store with
        | Proc((sl, env2, func)) -> 
            let (res, store1) = expList values env store
            let env3 = Map.ofList (List.zip sl res)
            stm func (join env2 env3) store 
        | _ -> failwith "error"

    | IfElse(p, s, f) ->
        let temp = match exp p env store with | (BoolVal b, _) -> b | _ -> failwith "error"
        if temp then (stm s env store) else (stm f env store)
    | IfThen(p, s) ->
        let temp = match exp p env store with | (BoolVal b, _) -> b | _ -> failwith "error"
        if temp then (stm s env store) else (None, store)
    
and decList ds env store = 
    match ds with
    | []       -> (env,store)
    | d::drest -> let (env1,store1) = dec d env store
                  decList drest env1 store1

and dec d env store =
    match d with 
    | VarDec(s,e) -> let loc = nextLoc()
                     match exp e env store with
                     | (IntVal _ as res, store1)  
                     | (BoolVal _ as res, store1) 
                     | (StringVal _ as res, store1)  
                                                 -> let env2 = Map.add s (Reference loc) env
                                                    let store2 = Map.add loc (SimpVal res) store1
                                                    (env2, store2)
                     | _                         -> failwith "error"
    | Procedure(name, arg, func) ->
        let loc = nextLoc()
        let env2 = Map.add name (Reference loc) env
        let c:Closure = (List.map (fun x -> match x with | Var(s) -> s | _ -> failwith "error") arg, env, func)
        let store2 = Map.add loc (Proc c) store
        (env2, store2)            
    | RecProcedure(name, arg, func) ->
        let loc = nextLoc()
        let env2 = Map.add name (Reference loc) env
        let c:Closure = (List.map (fun x -> match x with | Var(s) -> s | _ -> failwith "error") arg, env2, func)
        let store2 = Map.add loc (Proc c) store
        (env2, store2)           
    | VarArr(name, size, value) ->
        let loc = nextLoc()    
        let env2 = Map.add name (Reference loc) env
        let length = match exp size env store with | (IntVal(v), _) -> v | _ -> failwith "error"
        let arr = Array.init length (fun _ -> fst (exp value env store))
        let store2 = Map.add loc (ArrayCnt arr) store
        (env2, store2)
