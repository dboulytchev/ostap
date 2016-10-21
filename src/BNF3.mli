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

(** BNF tree implementation. *)

(** BNF expression implementation. *)
module Expr :
  sig

    (** Main type. *)
    type t =
	String  of string                        (** String terminal                           *)
      | Term    of string                        (** Non-string terminal                       *)
      | Nonterm of string                        (** Nonterminal                               *)
      | Apply   of t * t list                    (** Rule application                          *)
      | Star    of t                             (** Iteration                                 *)
      | Plus    of t                             (** Non-empty iteration                       *)
      | Opt     of t                             (** Optional expression                       *)
      | Alt     of t list                        (** Alternation                               *)
      | Seq     of t list                        (** Sequencing                                *)
      | Group   of t                             (** Expression in brackets                    *)
      | Custom  of [`S of string | `T of t] list (** Custom mixed structure (for internal use) *)

    (** {1 Constructors} *)

    (** String terminal. *)
    val string : string -> t

    (** Non-string terminal. *)
    val term : string -> t

    (** Nonterminal. *)
    val nonterm : string -> t

    (** Rule application. *)
    val apply : t -> t list -> t

    (** Iteration. *)
    val star : t -> t
         
    (** Non-empty iteration. *)
    val plus : t -> t

    (** Optional expression. *)
    val opt : t -> t

    (** Alternation. *)
    val alt : t list -> t
     
    (** Sequencing. *)
    val seq : t list -> t

    (** Expression in brackets. *)
    val group : t -> t

    (** Custom text. *)
    val custom : [`S of string | `T of t] list-> t

    (** TeX printer. *)
    val toTeX : t -> string

    (** Tree printer. *)
    val toTree : t -> string
    
  end

(** Rule definition representation. *)
module Def :
  sig

    (** Main type *)
    type t 

    (** Constructor of simple definition. Takes name and body. *)
    val make : string -> Expr.t -> t

    (** Constructor of parameterized definition. Takes name, 
        argument name list and body. 
    *)
    val makeP : string -> string list -> Expr.t -> t

    (** TeX printer. *)
    val toTeX : t -> string

  end
