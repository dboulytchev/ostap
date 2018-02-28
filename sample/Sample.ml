(*
 * Sample: Ostap sample.
 * Copyright (C) 2006-2009
 * Dmitri Boulytchev, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(* This module serves as Ostap tutorial. The important part is expression in the
   form ostap (...) (see below), which specified the parsers using a certain
   syntax extension
*)
open Ostap
open Matcher

(* Supplementary wrapper: takes a parser, a printer, a string, parses the
   string with the parser, and prints the result (if any) with the printer
*)
let parse p pr s =
  match Util.parse
          (* Makes some default stream with minimal entries *)
          (object
             inherit Matcher.t s
             inherit Util.Lexers.decimal s
             inherit Util.Lexers.ident [] s                                      
             inherit Util.Lexers.skip [
	       Matcher.Skip.whitespaces " \t\n";
	       Matcher.Skip.lineComment "--";
	       Matcher.Skip.nestedComment "(*" "*)"
             ] s
           end)
          (ostap (p -EOF))
  with
  | `Ok   p  -> Printf.printf "Parsed      : %s\n" @@ pr p
  | `Fail er -> Printf.printf "Syntax error: %s\n" er

(* Supplementary printer combinators *)
let id     x               = x               
let pair   f g   (x, y)    = f x ^ g y       
let triple f g h (x, y, z) = f x ^ g y ^ h z 
let list   f x             = List.fold_left (^) "" @@ List.map f x 
let option f               = function None -> "None" | Some x -> "Some " ^ f x 

(* An overview of the basic combinators:
    - sequencing
    - attaching semantics
    - alternation
    - optionality
    - iteration
*)                              
let _ =
  (* Sequencing *)
  parse (ostap ("a" "b" "c")) (* several items in a row return a tuple of parse values *)
        (triple Token.repr Token.repr Token.repr)
        "abc";
  parse (ostap ("a" "b" "d")) (* should be parsing error --- "d" is expected, but "c" specified *)
        (triple Token.repr Token.repr Token.repr)
        "abc";
  parse (ostap ("a" -"b" "c")) (* an item can be skipped from the result by preceding it with "-" *)
        (pair Token.repr Token.repr)
        "abc";
  parse (ostap ("a" -"b" "c")) (* but the skipped item nevertheless important for parsing *)
        (pair Token.repr Token.repr)        
        "adc";
  parse (ostap (-"(" "a" -")")) (* it is quite useful to omit a non-important symbols from *)
                                (* being returned as a result                               *)
        Token.repr
        "(a)";
  parse (ostap (-"(" "a" -"b" -")")) (* by the way, whitespaces are allowed anywhere (due to a proper *)
                                     (* stream definition, see object ... end expression above        *)
        Token.repr
        " ( a  (* comment *)  b (* comment again *) )     ";
  parse (ostap ("a" "b" "c" {"nothing"})) (* now, a semantics can be specified; it replaces the default *)
                                          (* return value                                               *)
        id
        "a b c";
  parse (ostap (x:"a" y:"b" z:"c" {"Something: " ^ (Token.repr x) ^ (Token.repr y) ^ (Token.repr z)})) (* the semantics can make use of a bindings *)
        id
        "a b c";
  parse (ostap ("a" | "b")) (* besides sequencing, there is an alternation *) 
        Token.repr
        "a";
  parse (ostap ("a" | "b")) (* both branches are matched *) 
        Token.repr
        "b";
  parse (ostap (x:("a" | "b") {"Something: " ^ Token.repr x})) (* grouping is allowed *) 
        id
        "a";
  parse (ostap ("a"?)) (* optional parsing *)
        (option Token.repr)
        "a";
  parse (ostap ("a"?)) (* optional parsing *)
        (option Token.repr)
        "";
  parse (ostap (x:"a"? {match x with None -> "None" | Some s -> "Some " ^ Token.repr s})) (* bindings for optional return option type *)
        id
        "a";
  parse (ostap ("a"*)) (* greedy iteration (zero-or-more) *)
        (list Token.repr)
        "aaa";
  parse (ostap ("a"+)) (* greedy iteration (one-or-more) *)
        (list Token.repr)
        "aaa";
  parse (ostap (("a" | "b" | "c")+)) (* of course any parser can be specified as an argument *)
        (list Token.repr)
        "aa";
  parse (ostap (!(Util.list)[ostap ("a" | "b" | "c")])) (* parsers can be parameterized; ostap expressions can be nested; arbitrary expression can be emdedded *)
                                                        (* via the !(...) construct; there are some useful combinators in the Util module --- this one for     *)
                                                        (* non-empty lists                                                                                     *)
        (list Token.repr)
        "a, a, b, c, a, b";
  parse (ostap (!(Util.listBy)[ostap (";")][ostap ("a" | "b" | "c")])) (* another useful one: a list with explicitly specified delimiter *)
        (list Token.repr)
        "a; a; b; c; a; b";
  parse (ostap (x:("a" | "b" | "c") - $(Token.repr x) {"two " ^ Token.repr x ^ "'s"})) (* a $(<string-expression>) can be used to parse a dynamically-evaluated *)
                                                                                       (* string; we used "-" to omit the second occurrence of x from being     *)
                                                                                       (* returned; note a space between the "-" and the "$(...)"               *)
        id
        "aa";;

(* This completes the basic combinators overview *)

(* Among the expression ostap (...) there is a structure item ostap (...) *)
(* to specify a set of mutally-recursive definitions:                     *)
  
ostap (
  animal  : "zeboro" | "cat" | "rabbit" | "giraffe";
  location: "pool" | "bank" | "cafe" | "theater";
  action  : "playing" | "eating" | "swimming" | "working";
  phrase  : "A" animal "is" action "in" "a" location {"parsed"}                                                               
)

let _ = List.iter (parse phrase id) ["A zeboro is playing in a theater";
                                     "A cat is eating in a cafe";
                                     "A rabbit is eating a cat"];;

(* Now some "real" parsers *)
type expr = Mul of expr * expr | Add of expr * expr | Var of string

let rec expr_to_string = function
| Var s -> s
| Mul (x, y) -> "(" ^ expr_to_string x ^ " * " ^ expr_to_string y ^ ")"
| Add (x, y) -> "(" ^ expr_to_string x ^ " + " ^ expr_to_string y ^ ")"

(* Expressions with right-associative binaries *)                                                                      
module RightAssoc =
  struct
    
    ostap (
      expr   : addi;                                                                        
      addi   : x:mulli "+" y:addi {Add (x, y)} | mulli; 
      mulli  : x:primary "*" y:mulli {Mul (x, y)} | primary;
      primary: x:IDENT {Var x} | -"(" expr -")"
    )      

    let _ = List.iter (parse expr expr_to_string)  ["a"; "a+b"; "a+b+c"; "a+b*c"; "a+b*c+d"; "(a+b)*c"]
  
  end                             

(* Expressions with left-associative binaries *)                                                                      
module LeftAssoc =
  struct
    
    ostap (
      expr   : addi;                                                                        
      addi   : <x::xs> :!(Util.listBy)[ostap ("+")][mulli]   {List.fold_left (fun x y -> Add (x, y)) x xs}; (* note the use of a pattern for bindings; *)
      mulli  : <x::xs> :!(Util.listBy)[ostap ("*")][primary] {List.fold_left (fun x y -> Mul (x, y)) x xs}; (* note a space between the "<...>" and the ":" *) 
      primary: x:IDENT {Var x} | -"(" expr -")"
    )      

    let _ = List.iter (parse expr expr_to_string)  ["a"; "a+b"; "a+b+c"; "a+b*c"; "a+b*c+d"; "(a+b)*c"]
  
  end

(* But better use a custom combinator Util.expr: *)
module ExprExample =
  struct

    ostap (
      expr:
  	!(Util.expr                                         (* The combinator Util.expr takes three arguments:                                                      *)
           (fun x -> x)                                     (* --- a function, used to transform each parsed subexpression into something; often just an id         *)
           [|                                               (* --- an array of binary operator specifiers, ordered by the priority in increasing order              *)
             `Lefta , [ostap ("+"), fun x y -> Add (x, y)]; (*     --- each specifier describes the associativity at given priority (one of `Lefta, `Righta, `Nona) *)
             `Righta, [ostap ("*"), fun x y -> Mul (x, y)]; (*     --- and the list of pairs:                                                                       *)
           |] 	                                            (*         --- the parser for the operator's infix and two-argument function to construct AST node      *)
           primary                                          (* --- a parser for the primary (simplest form of the expression)                                       *)
         );
      
      primary: x:IDENT {Var x} | -"(" expr -")"
    )

    let _ = List.iter (parse expr expr_to_string)  ["a"; "a+b"; "a+b+c"; "a+b*c"; "a+b*c*e+d"; "(a+b)*c"]

  end

(* And now some "real-world" example *)
module ShallowLanguageImplemenation =
  struct

    let empty  x       = failwith @@ "Undefined variable " ^ x
    let update x v s y = if x = y then v else s y
                                                
    let runParser p s =
      match Util.parse
          (object
             inherit Matcher.t s
             inherit Util.Lexers.decimal s
             inherit Util.Lexers.ident ["if"; "then"; "else"; "fi"; "while"; "do"; "done"] s                                      
             inherit Util.Lexers.skip [
	       Matcher.Skip.whitespaces " \t\n";
	       Matcher.Skip.lineComment "--";
	       Matcher.Skip.nestedComment "(*" "*)"
             ] s
           end)
          (ostap (p -EOF))
      with
      | `Ok   p  -> p
      | `Fail er -> failwith @@ Printf.sprintf "Syntax error: %s\n" er

    ostap (
      expr:
        !(Util.expr
           (fun x -> x)
           [|
             `Nona  , [ostap ("=="), (fun x y s -> if x s = y s then 1 else 0)];
             `Lefta , [ostap ("+" ), (fun x y s -> x s + y s); ostap ("-"), (fun x y s -> x s - y s)];
             `Lefta , [ostap ("*" ), (fun x y s -> x s * y s); ostap ("/"), (fun x y s -> x s / y s)]
           |]
           primary
         );
      
      primary: x:IDENT {fun s -> s x} | n:DECIMAL {fun s -> n} | -"(" expr -")";

      simple_stmt:
        x:IDENT ":=" e:expr                            {fun s -> update x (e s) s}
      | "if" c:expr "then" s1:stmt "else" s2:stmt "fi" {fun s -> (if c s = 0 then s2 else s1) s}
      | "while" c:expr "do" s1:stmt "done"             {fun s -> let rec w s = if c s = 0 then s else w (s1 s) in w s};

      stmt: <s::ss> : !(Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss d -> ss @@ s d) s ss}
    )
    
    let fact =
      let f = runParser stmt "result := 1; while 1 - (n == 0) do result := result * n; n := n - 1 done" in
      fun n -> (f @@ update "n" n empty) "result"

    let _ = List.iter (fun n -> Printf.printf "fact %d = %d\n" n (fact n)) [1; 2; 3; 4; 5; 6; 7]
                                              
  end
  

