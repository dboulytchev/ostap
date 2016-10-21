(*
 * Util: predefined Ostap utilities.
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

open Types
open Combinators

(** Predefined parsing utilities. *)

(** {2 Parsing keywords} *)

(** [keyword[string]] parses a string as a keyword (e.g. taking into
    account word boundaries
*)
val keyword : string -> < regexp : string -> string -> 'a; .. > -> 'a

(** Infix synonym for [keyword] *)
val ( ~$ ) : string -> < regexp : string -> string -> 'a; .. > -> 'a

(** {2 List parsing} *)

(** [listByWith s delim item f x] parses a non-empty list of [item]s delimited by [delim] from a stream [s]
    and folds it with function [f] and initial value [x]. Note that inside Ostap syntax extension the notation 
    [listByWith[delim][item][f][x]] should be used.
 *)
val listByWith : ('a, 'b, < add : 'c -> 'c; .. > as 'c) Types.parse ->
                 ('a, 'd, 'c) Types.parse ->
                 ('e -> 'd -> 'e) -> 
                 'e -> 
                 'a ->
                 ('a, 'e, 'c) Types.result 

(** [listBy s delim item] parses a non-empty list of [item]s delimited by [delim] from a stream [s].
     Note that inside Ostap syntax extension the notation [listBy[delim][item]] should be used.
  *)
val listBy : ('a, 'b, < add : 'c -> 'c; .. > as 'c) parse -> 
             ('a, 'd, 'c) parse -> 
             'a -> 
             ('a, 'd list, 'c) Types.result

(** [list s item] parses a non-empty list delimited by commas. Inside Ostap syntax extensions this should
     be used in the form [list[item]].
 *)
val list : ('a, 'd, 'c) parse -> 
           (< look : string -> ('a, 'b, < add : 'c -> 'c; .. > as 'c) result; .. > as 'a) ->
           ('a, 'd list, 'c) result

(** [listWith s item f x] parses a non-empty list delimited by commas and folds it with the function [f] and initial
    value [x]. Inside Ostap syntax extensions this should be used in the form [listWith[item][f][x]].
 *)
val listWith : ('a, 'd, 'c) Types.parse -> 
               ('e -> 'd -> 'e) -> 'e -> 
               (< look : string -> ('a, 'b, < add : 'c -> 'c; .. > as 'c) Types.result; .. > as 'a) ->  
               ('a, 'e, 'c) Types.result

(** [list0*] functions are that analoguous to [list*] but parse possibly empty lists. *)
val list0ByWith : ('a, 'b, < add : 'c -> 'c; .. > as 'c) Types.parse ->
                  ('a, 'd, 'c) Types.parse ->
                  ('e -> 'd -> 'e) -> 
                  'e -> 
                  'a ->
                  ('a, 'e, 'c) Types.result 

val list0By : ('a, 'b, < add : 'c -> 'c; .. > as 'c) parse -> 
              ('a, 'd, 'c) parse -> 
              'a ->
              ('a, 'd list, 'c) Types.result

val list0With : ('a, 'd, 'c) Types.parse -> 
                ('e -> 'd -> 'e) -> 
                'e -> 
                (< look : string -> ('a, 'b, < add : 'c -> 'c; .. > as 'c) Types.result; .. > as 'a) ->  
                ('a, 'e, 'c) Types.result

val list0 : ('a, 'd, 'c) parse -> 
            (< look : string -> ('a, 'b, < add : 'c -> 'c; .. > as 'c) Types.result; .. > as 'a) ->
            ('a, 'd list, 'c) result

(** Identity parser *)
val id : ('a, 'b, 'c) parse -> ('a, 'b, 'c) parse

(** {2 Expression parsing} *)

(** Expression parser generator. [expr f opers opnd] constructs parser of expressions with (ground) operands
    specified by [opnd] and operators specified by [opers]. Operand specification [opnd] has to be
    parser of type [('c, 'a, 'e) parse], where ['c] is the type of stream, ['a] is the type of
    operand (and therefore the type of expression) and ['e] is the type of reason. Operator specification
    [opers] is represented by an array in which binary operators with the same precedence level and the same
    associativity are grouped together (the first array element corresponds to the group of operators with the
    lowest priority). Each elements of the array contains pair [(assoc, oplist)] where [assoc] is
    associativity value ([`Righta], [`Lefta] or [`Nona]) and [oplist] is a list of pairs [(opparse, opsema)], where
    [opparse] is parser of operator symbol and [opsema] is semantic function of type ['a -> 'a -> 'a]. Additional
    higher-order parser [f] is used to allow some post-processing of every subexpression being parsed. In most 
    cases identity parser [id] is sufficient.

    Example:
    
    {[
      let rec parse s =                                                                                    
        expr id
          [|                                                                                            
            left , [ostap ("+"), (fun x y -> `Add (x, y)); ostap ("-"), (fun x y -> `Sub (x, y))]; 
            left , [ostap ("*"), (fun x y -> `Mul (x, y)); ostap ("/"), (fun x y -> `Div (x, y))]  
          |]                                                                                            
          primary                                                                                       
          s                                                                                             
      and ostap (primary:  n:ident \{`Ident n} | -"(" parse -")")                                        
    ]}

    The example above defines parser of expressions with left-associative operators ["+"],  ["-"], ["*"], and ["/"].
    First two operators have lower precedence level than others. Identifier and expressions in brackets
    can be used as operands.
 *)
 val expr :
     (('a, 'b, < add : 'c -> 'c; .. > as 'c) parse -> ('a, 'b, 'c) parse) ->
     ([ `Lefta | `Nona | `Righta ] * (('a, 'd, 'c) parse * ('b -> 'b -> 'b)) list) array ->
     ('a, 'b, 'c) parse ->
     ('a, 'b, 'c) parse

(** {2 Lexing components} *)

module Lexers :
  sig

    (** A lexer component for parsing identifiers, started with an uppercase letter;
        the first argument is a list of keywords, the second --- a string to parse
     *)
    class virtual uident : string list -> string ->
      object('a)
        method virtual get : string -> Re_str.regexp -> ('a, Matcher.Token.t, Reason.t) Types.result
        method getUIDENT : ('a, string, Reason.t) Types.result
      end

    (** A lexer component for parsing identifiers, started with a lowercase letter;
        the first argument is a list of keywords, the second --- a string to parse
     *)
    class virtual lident : string list -> string ->
      object('a)
	method virtual get : string -> Re_str.regexp -> ('a, Matcher.Token.t, Reason.t) Types.result
        method getLIDENT : ('a, string, Reason.t) Types.result
      end

    (** A lexer component for parsing regular identifiers;
        the first argument is a list of keywords, the second --- a string to parse
     *)
    class virtual ident : string list -> string ->
      object('a)
        method virtual get : string -> Re_str.regexp -> ('a, Matcher.Token.t, Reason.t) Types.result
        method getIDENT : ('a, string, Reason.t) Types.result
      end

    (** A lexer component for parsing signed decimal constants; the first argument is 
        a string to parse
     *)
    class virtual decimal : string ->
      object('a)
	method virtual get : string -> Re_str.regexp -> ('a, Matcher.Token.t, Reason.t) Types.result	
        method getDECIMAL : ('a, int, Reason.t) Types.result
      end

    (** A lexer component for parsing quoted strings; the first argument is 
        a string to parse
     *)
    class virtual string : String.t ->
      object('a)
	method virtual get : String.t -> Re_str.regexp -> ('a, Matcher.Token.t, Reason.t) Types.result	
        method getSTRING : ('a, String.t, Reason.t) Types.result
      end

    (** A lexer component for skipping whitespaces and comments; the first argument is
        a list of skipped items specifiers (see Matcher.mli), the second --- a string 
        to parse
     *)
    class skip : Matcher.Skip.t list -> String.t ->
      object ('a) inherit Matcher.t end

  end

(** {2 Miscellaneous} *)

(** [read fname] returns the content of the file [fname]. *)
val read : string -> string

(** [parse l p] parses a stream, represented by [l], by a parser [p]; this is a 
    simplified entry point 
*)
val parse : (#Matcher.t as 'a) -> ('a, 'c, Reason.t) parse -> [`Fail of String.t | `Ok of 'c]
