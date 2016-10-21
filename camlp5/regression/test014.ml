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
  n: 
     c:CONST {`N c} ;

  t:
     -"(" a:memo[e] -")" {`TBr a}
   | a:memo[n] {`TN a} ;

  e: 
     a:memo[e] -"+" b:memo[t] {`EAdd (a, b)}
   | a:memo[e] -"-" b:memo[t] {`ESub (a, b)}
   | a:memo[t] {`ET a} ;

  main: memo[e] -EOF
)

let _ =
  let rec print r = 
    match r with 
    | `N _ -> "n"
    | `TN a -> "T[" ^ (print a) ^ "]"
    | `TBr a -> "T[" ^ "(" ^ (print a) ^ ")" ^ "]"
    | `ET a -> "E[" ^ (print a) ^ "]"
    | `EAdd (a, b) -> "E[" ^ (print a) ^ "+" ^ (print b) ^ "]"
    | `ESub (a, b) -> "E[" ^ (print a) ^ "-" ^ (print b) ^ "]"
  in
  match main (new lexer "1-(2+3)+4") with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed m -> Printf.printf "Not parsed:\n%s\n" (Reason.toString `All `Acc m)
