open Ostap
open Types
open Matcher 
open Printf 


class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let const = Re_str.regexp "[0-9]+" in
  object (self)

    inherit Combinators.memoStream s

    method skip p c = skip s p c
    method getCONST = self#get "constant"   const

  end

let memo : (lexer, 'p, 'e) parse -> lexer -> (lexer, 'p, 'e) result = fun p s -> 
  s#memoize p

ostap (
  primary: 
     c:CONST {`N c} ;

  inner:
     i:memo[inner] -"-" p:primary {`I2 (i, p)}
   | p:primary {`I1 p} ;

  exp: 
     i:memo[inner] -"+" e:memo[exp] {`E2 (i, e)}
   | i:memo[inner] {`E1 i} ;

  main: memo[exp] -EOF
)

let _ =
  let rec print r = 
    match r with 
    | `N p -> "n"
    | `I1 i -> "M[" ^ (print i) ^ "]"
    | `I2 (i, p) -> "M[" ^ (print i) ^ "-" ^ (print p) ^ "]"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
  in
  let run input = 
    match main (new lexer input) with
    | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
    | Failed m -> Printf.printf "Not parsed:\n%s\n" (Reason.toString `All `Acc m)
  in
  let input0 = "1+2+3" in
  let input1 = "1-2-3" in
  run input0;
  run input1 

