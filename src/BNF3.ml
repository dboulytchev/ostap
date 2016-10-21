(*
 * BNF3: BNF tree representation.
 * Copyright (C) 2008
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

open Printf

module type PrintHelper =
  sig

    type t
    
    val str    : string -> string
    val term   : string -> string
    val nt     : string -> string
    val aster  : string -> string
    val plus   : string -> string
    val alt    : string list -> string
    val seq    : string list -> string
    val group  : string -> string
    val custom : (t -> string) -> [`S of string | `T of t] list -> string
    val apply  : string -> string list -> string
    val opt    : string -> string
    val list   : (t -> string) -> t list -> string list
    val rule   : string -> string -> string
    val prule  : string -> string list -> string -> string
	
  end

let concatWith s f x y = (if x = "" then "" else x ^ s) ^ (f y)
let concat       f x y = concatWith ", " f x y
let id             x   = x
let concat'        x y = concat id x y
let fold         f x   = List.fold_left f "" x

module rec Expr :
  sig

    type t =
	String  of string   
      | Term    of string    
      | Nonterm of string    
      | Apply   of t * t list 
      | Star    of t        
      | Plus    of t       
      | Opt     of t       
      | Alt     of t list  
      | Seq     of t list   
      | Group   of t      
      | Custom  of [`S of string | `T of t] list

    val string  : string -> t
    val term    : string -> t
    val nonterm : string -> t
    val apply   : t -> t list -> t
    val star    : t -> t
    val plus    : t -> t
    val opt     : t -> t
    val alt     : t list -> t
    val seq     : t list -> t
    val group   : t -> t
    val custom  : [`S of string | `T of t] list -> t

    val toTeX  : t -> string
    val toTree : t -> string

  end =
  struct

    type t =
	String  of string
      | Term    of string
      | Nonterm of string
      | Apply   of t * t list
      | Star    of t
      | Plus    of t
      | Opt     of t
      | Alt     of t list
      | Seq     of t list
      | Group   of t
      | Custom  of [`S of string | `T of t] list

    let alt = function
      | [x] -> x
      |  x  -> Alt x

    let seq = function
      | [x] -> x
      |  x  -> Seq x

    let string x = String x

    let term s = Term s

    let nonterm s = Nonterm s

    let apply x y = Apply (x, y)

    let star x = Star x

    let plus x = Plus x

    let opt = function
      | Group x -> Opt x
      | Opt   x -> Opt x
      | x       -> Opt x

    let group = function
      | ((Seq _) as x) | ((Alt _) as x) -> Group x
      | x -> x

    let custom x = Custom x
	  
    module Printer (X : PrintHelper with type t = t) =
      struct

	let rec print = function 
	  | String   s     -> X.str     s
	  | Term     t     -> X.term    t
	  | Nonterm  n     -> X.nt      n
	  | Star     e     -> X.aster  (print e)
	  | Plus     e     -> X.plus   (print e)
	  | Opt      e     -> X.opt    (print e)
	  | Alt      l     -> X.alt    (List.map print l)
	  | Seq      l     -> X.seq    (List.map print l)
	  | Group    e     -> X.group  (print e)
	  | Custom   s     -> X.custom  print s
	  | Apply   (x, y) -> X.apply  (print x) (X.list print y)
		
      end

    module TeXPrinter  = Printer (TeXHelper)
    module TreePrinter = Printer (TreeHelper)
     
    let toTree = TreePrinter.print
    let toTeX  = TeXPrinter .print
	    
  end
and TreeHelper : PrintHelper with type t = Expr.t =
  struct

    type t = Expr.t
	  
    let opt    str   = sprintf "Opt (%s)" str
    let plus   str   = sprintf "Plus (%s)" str
    let aster  str   = sprintf "Aster (%s)" str
    let group  str   = sprintf "Group (%s)" str
    let nt     str   = sprintf "Nonterm %s" str
    let alt    lst   = sprintf "Alt (%s)" (fold concat' lst)
    let seq    lst   = sprintf "Seq (%s)" (fold concat' lst)
    let list   f x   = List.map f x
    let term   str   = sprintf "Term %s" str
    let str    arg   = sprintf "String %s" arg
    let rule   x y   = sprintf "%s :: %s" x y

    let prule  x y z = 
      let y = List.fold_left (fun acc y -> acc ^ "[" ^ y ^ "]") "" y in
      sprintf "%s%s :: %s" x y z

    let apply  x y   = sprintf "Apply (%s, [%s])" x (fold (concat id) y)
    let custom f x   = sprintf "Custom (%s)" (fold (concatWith "" (function `S s -> s | `T t -> f t)) x)

  end
and TeXHelper : PrintHelper with type t = Expr.t =
  struct
	
    type t = Expr.t
	  
    let quote s =
      let buf = Buffer.create (String.length s * 2) in
      for i=0 to String.length s - 1 do
	Buffer.add_string buf
	  (match s.[i] with
	  | '"'  -> "\\\""
	  | '{'  -> "\\{"
	  | '}'  -> "\\}"
	  | '$'  -> "\\$"
	  | '&'  -> "\\&"
	  | '#'  -> "\\#"
	  | '%'  -> "\\%"
	  | '_'  -> "\\_"
	  | '~'  -> "$\\tilde{}$"
	  | '\\' -> "$\\backslash$"
	  | '<'  -> "$<$"
	  | '>'  -> "$>$"
	  | '|'  -> "$|$"
	  | '^'  -> "$\\hat{}$"
	  | c    -> String.make 1 c
	  )
      done;
      Buffer.contents buf 
	
    let opt    str = sprintf "\\osropt{%s}" str
    let plus   str = sprintf "\\osrplus{%s}" str
    let aster  str = sprintf "\\osraster{%s}" str
    let group  str = sprintf "\\osrgroup{%s}" str
    let nt     str = sprintf "\\osrnonterm{%s}" (quote str)
    let alt    lst = "\osfralt " ^ (fold (concatWith "\\osralt " id) lst)
    let seq    lst = sprintf "\\osrblock{%s}" (fold (concatWith "\\osbr " id) lst)
    let list   f x = List.map f x
    let term   str = sprintf "\\osrterm{%s}" (quote str)

    let str x  = 
      let f = ref true in
      for i=0 to String.length x - 1 do
	let c = x.[i] in
	f := !f && ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
      done;
      if !f then sprintf "\\osrterm{%s}" (quote x) 
      else sprintf "\\osrterm{``%s''}" (quote x)

    let rule   x y = sprintf "\\osrule{%s}{%s}\n" x y

    let prule  x y z = 
      let y = List.fold_left (fun acc yi -> acc ^ "[" ^ yi ^ "]") "" y in
      sprintf "\\osprule{%s}{%s}{%s}\n" x y z

    let custom f x = fold (concatWith "" (function `S s -> quote s | `T t -> f t)) x
    let apply  x y = sprintf "%s%s" x (fold (fun acc x -> acc ^ sprintf "\\osrargs{%s}" x) y)
	
  end

module Def =
  struct

    type t = string * string list * Expr.t

    let make  name      = function Expr.Group x -> name, []  , x | x -> name, []  , x
    let makeP name args = function Expr.Group x -> name, args, x | x -> name, args, x

    let rec toTeX (name, args, expr) =
      match args with
      | []   -> TeXHelper.rule  name (Expr.toTeX expr)
      | args -> TeXHelper.prule name args (Expr.toTeX expr)

  end

