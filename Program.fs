module Program

open AST
open System
open ParserUtil
open Interpreter


// Create an initial environment

let plusInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1+i2) );;
let minusInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1-i2) );;
let multInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1*i2) );;
let eqInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1=i2) );;
let neqInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1<>i2) );;
let lessEqInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1<=i2) );;
let lessInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1<i2) );;
let gen = let generator = new System.Random()
          generator.Next;;   
let randomInt = Primitive( fun [IntVal rng] -> IntVal (gen rng) );; 
let toString = let f vs =  match vs with 
                           | [IntVal v] -> StringVal(string v)
                           | [BoolVal v] -> StringVal(string v)
                           | [StringVal v] -> StringVal v
                           | _          -> failwith "error"
               Primitive f;;

let initEnv = Map.ofList [("+",plusInt); ("-",minusInt); ("*",multInt); ("=",eqInt); ("<>",neqInt); ("<=",lessEqInt); ("<",lessInt); 
                           ("randomInt", randomInt); ("toString",toString)  ];;


// Parse a program in a file  
let fac = parseFromFile "Factorial1.while"

// Interpret the program 
//let _  = stm fac initEnv Map.empty;;

let p5 = parseFromFile "Factorial3.while";;
//let _  = stm p5 initEnv Map.empty;;
let res_p6 = parseFromFile "Factorial4.while";;

let randomArray = parseDec "proc randomArray(rng, lng) 
                               let a[!lng]: 0;
                                   i: 0
                               in while <(!i,a.length)
                               do a[!i] := randomInt(rng);
                                  i    := +(!i,1)
                               od;
                               return a
                               end";;
//printfn "%A" randomArray

let arrayUtilDecs = parseDecListFromFile "ArrayUtil.while";;

let (basisEnv, basisStore) = decList arrayUtilDecs initEnv Map.empty;; 

let ap1 = parseFromFile"ArrayProg1.while";; 
let _ = ignore (stm ap1 basisEnv basisStore);;

let ap2 = parseFromFile"ArrayProg2.while";; 
let _ = ignore (stm ap2 basisEnv basisStore);;

let test1 = parseFromFile "Example1.while";;

printfn "%A\n\n" test1 

let _ = ignore (stm test1 basisEnv basisStore);;

let test2 = parseFromFile "Example2.while";;

printfn "%A\n\n" test1 

let _ = ignore (stm test2 basisEnv basisStore);;
