(*
 * PrioReorderer: reordering expression trees by priorities.
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

(** Reordering expression in according to priorities of its operators. *)

(** Signature to abstract expression being reordered. *)
module type Expression =
  sig

    (** The type of expression. *)
    type t              
   
    (** Discovers the kind of expression: either infix with priority and 
        two arguments or other kind of expression.
    *)
    val discover : t -> [`Infix of int * t * t | `Other]
 
    (** [replace expr x y] replaces arguments of infix expression [expr]
        with [x] and [y] 
    *)
    val replace : t -> t -> t -> t

    (** Map function for expressions. *)
    val map : (t -> t) -> t -> t       
	  
  end

(** Functor to instantiate reorderer. *)
module Make (E : Expression) :
  sig 

    (** Reordering function. *)
    val sort : E.t -> E.t 

  end
