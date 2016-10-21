(*
 * Matcher: simple lexer pattern.
 * Copyright (C) 2006-2008
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

(** Implementation of streams as objects. *)

(** {2 General description} *)

(** This module provides a sample pattern for implementation of streams as objects.
    A stream is represented by a number of methods each of which tries to
    consume a certain lexeme from the stream. Thought [Ostap] is generally
    invariant with regard to stream implementation this approach helps to abstract
    parsers from lexers. The main use case for the implementation is to
    provide lexers for parser functions created via [Pa_ostap] syntax extensions,
    but you may use it on your own.
 
    The pattern given below is in turn just a sample --- it implements some
    simplest way to produce lexers using [Str] standard library; additionally
    it predefines types for error signaling and token representation. You must
    not generally use these types since you may implement objective lexer
    in any way you want. For example, you may write a grammar expression, say

    [let p = ostap (IDENT "(" IDENT (-"," IDENT)* ")")]
 
    inspect its type 

    {[
     (< getIDENT : ('a, 'b, 'c) Types.result;           
        look     : string -> ('a, 'd, 'c) Types.result; 
        ..                                                    
      > as 'a) ->                                             
     ('a, 'b * 'd * 'b * 'b list * 'd, 'c) Types.result 
    ]}

    and conclude that you need a stream of type

    {[
     < getIDENT : ('a, 'b, 'c) Types.result;           
       look     : string -> ('a, 'd, 'c) Types.result; 
       ..                                                    
     > as 'a                                                 
    }]

    Here ['a] is the type of stream itself, ['b] --- the type of data, returned by successful
    application of [getIDENT], ['c] --- the type of data returned by error/failure, ['d] ---
    the type of data, returned by successful application of [look]. [look] is just a member
    function to match a string against the stream.

    You may easily provide an appropriate stream implementation by inheriting class [matcher]. 
    The detailed description is given in the following section; for now we provide a simple example 
    for the last grammar expression:

    {[
     open Matcher 
	
     class lexer s = v}
         let ident = Re_str.regexp "[a-zA-Z_]\([a-zA-Z_0-9]\)*"] in 
         let skip  = Skip.create [Skip.whitespaces " \t\n\r"] in 
         object (self) 
            inherit Matcher.t s 
            method skip p coord = skip s p coord 
            method getIDENT = self#get "identifier" ident 
         end 
    ]}
       
    Lexer is an immutable object sprang over the string [s] to parse. Additionally it
    maintains the position within the string and its coordinate. Each time the lexeme is consumed
    from the stream the position and coordinates are shifted and a fresh instance
    of lexer is returned as the residual stream.

    To turn [matcher] into lexer you may need to define the following methods:

    {ul
       {- method [skip] to skip meaningless symbols (whitespaces, comments etc.) from 
        the stream; in the default case it does not skip anything.}
       {- method [getL] for any lexeme [L] to recognize. You may do this simply by
        calling method [get] from the base class and passing to it string name of the
        lexeme (for diagnostic purposes) and regular pattern of type [Re_str.regexp].
       } 
    }    

    [matcher] refers to three predefined types: [Msg.t], [Token.t], [Msg.Coord.t]. If you are
    not satisfied with them please consider implementing your own matcher.   
*)

(** {2 Simple implementation of streams as objects} *)

(** Token: a string augmented with text coordinates. *)
module Token :
  sig
 
    (** Type of the token. *)
    type t = string * Msg.Coord.t

    (** String visualizer. *)
    val toString : t -> string

    (** Text coordinate. *)
    val loc : t -> Msg.Locator.t

    (** String image. *)
    val repr : t -> string

  end

(** [except s] makes regular expression to match any string which does not contain [s]
    as a contiguous substring.
*)
val except : string -> string
  
(** [checkPrefix prefix s p] returns true iff [s] at the position [p] starts with [prefix]. *)
val checkPrefix : string -> string -> int -> bool

val defaultSkipper : int -> Msg.Coord.t -> [`Skipped of int * Msg.Coord.t | `Failed of Msg.t]

(** Module to provide various skipping functions. *)
module Skip :
  sig

    (** Type of function to skip symbols. [t s p] returns either [`Skipped p], where [p] is
        the first position past skipped symbols, or [`Failed reason] if something went wrong.
    *)
    type t = string -> int -> [`Skipped of int | `Failed of string] 

    (** Makes comment skipper. For example, [comment "/*" "*/"] makes skipper to bypass
        C-style comments.
    *)
    val comment : string -> string -> t

    (** Makes nested comment skipper. For example, [nestedComment "(*" "*)"] makes skipper
        to bypass OCaml-style comments.
    *)
    val nestedComment : string -> string -> t

    (** Makes line comment skipper. For example, [lineComment "--"] makes skipper to bypass
        VHDL-style comments.
    *)
    val lineComment : string -> t

    (** Makes whitespace skipper (usually [whitespaces " \t\n\r"]). *)
    val whitespaces : string -> t

    (** Makes general skipper via combining several ones (for example, 
        [create [nestedComment "(*" "*)"; whitespaces " \n\t\r"]]).
	Returned skipper function shifts current position coordinates as well
    *)
    val create : t list -> (string -> int -> Msg.Coord.t -> [`Skipped of int * Msg.Coord.t | `Failed of Msg.t])

  end

(** Matcher pattern to inherit from to obtain the stream implementation. 
    [matcher s] creates an object that helps to match regular expressions against string [s]
*)
class t : string ->
  object ('a)

    (** Gets current position in string. *)
    method pos : int

    (** Gets current coordinate. *)
    method coord : Msg.Coord.t

    (** Gets current line. *)
    method line : int

    (** Gets current column. *)
    method col : int

    (** Gets prefix of current string symbols. *)
    method prefix : int -> string

    (** [get name expr] is a parser which parses regular expression [expr] at the current
        position. [name] is a name for diagnostic purposes.
    *)
    method get : string -> Re_str.regexp -> ('a, Token.t, Reason.t) Types.result

    (** [regexp name str] is a shorthand for [get name (Re_str.regexp str)]. *)
    method regexp : string -> string -> ('a, Token.t, Reason.t) Types.result

    (** [getEOF] detects the end of stream. *)
    method getEOF : ('a, Token.t, Reason.t) Types.result

    (** [loc] gets the current location in the stream. *)
    method loc : Msg.Locator.t

    (** [look str] looks at the current stream for string [str]. *)
    method look : string -> ('a, Token.t, Reason.t) Types.result

    (** Method to skip meaningless symbols (e.g. whitespaces); returns
        position and coordinates of first meaningful symbol. [skip] is implicitly
        called prior to all of the above methods except for the [getLAST].
    *)
    method skip : int -> Msg.Coord.t -> [`Skipped of int * Msg.Coord.t | `Failed of Msg.t]
    
    (*
    method changeSkip : (int -> Msg.Coord.t -> [`Skipped of int * Msg.Coord.t | `Failed of Msg.t]) -> 'a
    *)

  end
