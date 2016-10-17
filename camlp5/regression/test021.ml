open Ostap
open Types
open Combinators
open Matcher 
open Printf 

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let const = Str.regexp "[0-9]+" in
  object (self)
    inherit Combinators.memoStream s

    method skip p c = skip s p c
    method getCONST = self#get "constant" const
  end

module Expressions = struct 

  let rec e s = fix (fun e -> ostap ( x:e op:("+" | "-") y:t {`Binop (Token.repr op, x, y)}
                                    | t
                                    )) s
  and t s = fix (fun t -> ostap ( x:t op:("*" | "/") y:p {`Binop (Token.repr op, x, y)}
                                | p
                                )) s
  and p = ostap (c:CONST {`Const c} | -"(" x:e -")" {`Bracket x})
  let main = ostap (e -EOF)
  
  let rec print r = 
    match r with 
    | `Binop (op, x, y) -> sprintf "%s %s %s" (print x) op (print y)
    | `Bracket x -> sprintf "(%s)" (print x)
    | `Const c -> Token.toString c
end

module PlusMinusTree = struct
  let rec main = ostap (e -EOF)  
  and e s = fix (fun e -> ostap ( a:e "+" b:t {`EAdd (a, b)}
                                | a:e "-" b:t {`ESub (a, b)}
                                | a:t {`ET a} 
                                )) s
  and t = ostap ("(" a:e ")" {`TBr a} | a:n {`TN a})
  and n = ostap (a:CONST {`N a})

  let rec print r = 
    match r with 
    | `N _ -> "n"
    | `TN a -> "T[" ^ (print a) ^ "]"
    | `TBr a -> "T[" ^ " ( " ^ (print a) ^ " ) " ^ "]"
    | `ET a -> "E[" ^ (print a) ^ "]"
    | `EAdd (a, b) -> "E[" ^ (print a) ^ "+" ^ (print b) ^ "]"
    | `ESub (a, b) -> "E[" ^ (print a) ^ "-" ^ (print b) ^ "]"
end  
  
module InnerRecTree = struct
  let rec primary = ostap (c:CONST {`N c})
  and inner s = fix (fun inner -> ostap ( i:inner "-" p:primary {`I2 (i, p)}
                                        | p:primary {`I1 p} 
                                        )) s
  and exp s = fix (fun exp -> ostap ( i:inner "+" e:exp {`E2 (i, e)}
                                    | i:inner {`E1 i}
                                    )) s
  and main = ostap(memo[exp] -EOF)

  let rec print r = 
    match r with 
    | `N p -> "n"
    | `I1 i -> "M[" ^ (print i) ^ "]"
    | `I2 (i, p) -> "M[" ^ (print i) ^ "-" ^ (print p) ^ "]"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
end  

(* TODO what to do with this kind of greed? *)
module LValues = struct
  let rec l s = fix (fun x -> ostap (a:p "." "x" {`Lp a} | "x" {`Lx})) s
  and p s = fix (fun p -> ostap (a:p "(" "n" ")" {`Pn a} | a:p "." "x" {`Pl (`Lp a)} | "x" {`Pl `Lx})) s
  and main = ostap (l -EOF)
  
  let rec print r = 
    match r with 
    | `Lp a -> "L[" ^ (print a) ^ ".x]"
    | `Lx -> "L[x]"
    | `Pn a -> "P[" ^ (print a) ^ "(n)]"
    | `Pl a -> "P[" ^ (print a) ^ "]"
end

module Greed = struct 
  ostap (
    l: a:memo[p] -"b" {`Lp a} ;

    p: 
       a:memo[p] -"b" {`Pn a}
     | -"a" {`Pl } ;

    main: memo[l] -EOF 
  )
  
  let rec print r = 
    match r with 
    | `Lp a -> (print a) ^ "b"
    | `Pn a -> (print a) ^ "b"
    | `Pl -> "a"
end  
    
module GreedStar = struct 
  ostap (
    l: (-"b")* -"b" {`Lp } ;

    main: memo[l] -EOF 
  )
  
  let rec print r = 
    match r with 
    | `Lp -> "aha!"
end  

let _ =
  let run = fun parse input print ->
    match parse (new lexer input) with
    | Parsed ((b, _), _) -> Printf.printf "Parsed. %s\n" (print b)
    | Failed m -> Printf.printf "Not parsed:\n%s\n" (Reason.toString `All `Acc m) 
  in
  
  run (Expressions.main)   "1*2*3*4*5*6*7*8*9" (Expressions.print);
  run (Expressions.main)   "1+2*3*4-5*6/7*8+9" (Expressions.print);
  run (Expressions.main)   "123+345+567+789-1" (Expressions.print);
  run (Expressions.main)   "(3*(1-2))/10+12/6" (Expressions.print);
  run (Expressions.main)   "1*2*3*4*5*6*7*8* " (Expressions.print);
  
  run (PlusMinusTree.main) "1-(2+3)+4"         (PlusMinusTree.print);
  
  run (InnerRecTree.main)  "9-1-1"             (InnerRecTree.print);
  run (InnerRecTree.main)  "9+1+1"             (InnerRecTree.print);

  run (LValues.main)       "x(n)(n).x(n).x"    (LValues.print);
  run (LValues.p)          "x(n)(n).x(n).x"    (LValues.print);
  run (LValues.l)          "x(n)(n).x(n).x"    (LValues.print);
  
  run (Greed.main)         "bba"               (Greed.print);
  
  run (GreedStar.main)     "bba"               (GreedStar.print)
