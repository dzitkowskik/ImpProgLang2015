// Michael R. Hansen 03-01-2014

(* Load the parser and interpreter *)
#r "FSharp.PowerPack.dll"

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "ParserUtil.fs"
#load "Interpreter.fs"

open System
open Interpreter
open AST
open ParserUtil

// Functions for an initial environment

let plusInt = Primitive( function [IntVal i1; IntVal i2] -> IntVal(i1+i2) | _ -> failwith "Invalid arguments" );;
let minusInt = Primitive( function [IntVal i1; IntVal i2] -> IntVal(i1-i2) | _ -> failwith "Invalid arguments" );;
let multInt = Primitive( function [IntVal i1; IntVal i2] -> IntVal(i1*i2) | _ -> failwith "Invalid arguments" );;
let eqInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1=i2) | _ -> failwith "Invalid arguments" );;
let neqInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1<>i2) | _ -> failwith "Invalid arguments" );;
let lessEqInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1<=i2) | _ -> failwith "Invalid arguments" );;
let lessInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1<i2) | _ -> failwith "Invalid arguments" );;
let gen = let generator = new System.Random()
          generator.Next;;   
let randomInt = Primitive( function [IntVal rng] -> IntVal (gen rng) | _ -> failwith "Invalid arguments" );; 
let toString = let f vs =  match vs with 
                           | [IntVal v] -> StringVal(string v)
                           | [BoolVal v] -> StringVal(string v)
                           | [StringVal v] -> StringVal v
                           | _          -> failwith "error"
               Primitive f;;

let initEnv = Map.ofList [("+",plusInt); ("-",minusInt); ("*",multInt); 
                          ("=",eqInt); ("<>",neqInt); ("<=",lessEqInt); ("<",lessInt); 
                          ("randomInt", randomInt); ("toString",toString)  ];;

// Parsing strings
let s1 = parseStm "while <>(!n,0)
                   do y := *(!n,!y);
                      n := -(!n,1)
                   od";;

let s2 = parseStm "let n: 4; y: 1
                   in while <>(!n,0)
                      do y := *(!n,!y);
                         n := -(!n,1)
                      od
                   end";;

// Parsing from files
// Set current directory
System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;

let p3 = parseFromFile "Factorial1.while";;
// Interpret the statement
let _ = ignore (stm p3 initEnv Map.empty);;

let p4 = parseFromFile "Factorial2.while";;
let _ = ignore (stm p4 initEnv Map.empty);;

let p5 = parseFromFile "Factorial3.while";;
let _ = ignore (stm p5 initEnv Map.empty);;

let p6 = parseFromFile "Factorial4.while";;
let _ = ignore (stm p6 initEnv Map.empty);;

let p7 = parseFromFile "Factorial5.while";;
let _ = ignore (stm p7 initEnv Map.empty);;


// Parsing and interpreting programs with arrays

let randomArray = parseDec "proc randomArray(rng, lng) 
                               let a[!lng]: 0;
                                   i: 0
                               in while <(!i,a.length)
                               do a[!i] := randomInt(rng);
                                  i    := +(!i,1)
                               od;
                               return a
                               end";;


// Auxiliary procedures on arrays are in the file "ArrayUtil.while"
// They are used to built up a basic environment and a basic store
let arrayUtilDecs = parseDecListFromFile "ArrayUtil.while";;

let (basisEnv, basisStore) = decList arrayUtilDecs initEnv Map.empty;; 

let ap1 = parseFromFile"ArrayProg1.while";; 
let _ = ignore (stm ap1 basisEnv basisStore);;

let ap2 = parseFromFile"ArrayProg2.while";; 
let _ = ignore (stm ap2 basisEnv basisStore);;

let test1 = parseFromFile "Example1.while";;

let _ = ignore (stm test1 basisEnv basisStore);;

let test2 = parseFromFile "Example2.while";;

let _ = ignore (stm test2 basisEnv basisStore);;


