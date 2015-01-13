module ParserUtil

open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open AST
open Lexer
open Parser

// Parse a string. (A statement is parsed)  
let parseString (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.Main Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"


// Parse a file. (A statement is parsed) 
let parseFromFile filename =
   if File.Exists(filename)    
   then parseString(File.ReadAllText(filename))
   else invalidArg "ParserUtil" "File not found"

// Parse a string. (A declaration list is parsed)  
let parseDecList (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.DecList Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"

// Parse a file. (A declaration list is parsed) 
let parseDecListFromFile filename =
  if File.Exists(filename)    
   then parseDecList(File.ReadAllText(filename))
   else invalidArg "ParserUtil" "File not found"


let parseExp (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.Exp Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"


let parseDec (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.Dec Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"

let parseStm (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.Stm Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"

let parseStmList (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.StmList Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"