open Ostap
open Types
open Matcher 
open Printf 


class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let const = Str.regexp "[0-9]+" in
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

  exp: 
     e:memo[exp] -"+" p:primary {`E2 (e, p)}
   | p:primary {`E1 p} ;

  main: memo[exp] -EOF
)

let _ =
  let rec print r = 
    match r with 
    | `N p -> "n"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
  in
  match main (new lexer "1+2+3") with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed m -> Printf.printf "Not parsed:\n%s\n" (Reason.toString `All `Acc m)

