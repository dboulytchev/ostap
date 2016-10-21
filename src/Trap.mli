(*
 * Trap: connecting items to stream locations.
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

(** Trap is infrastructure to maintain the relation between parsed items and their locator 
    information.
*)

(** Signature of trapping module. *)
module type Sig =
  sig
    
    (** Type of trapped items. *)
    type t
	  
    (** [attach x loc] attaches [loc] to item [x]. *)
    val attach : t -> Msg.Locator.t -> unit

    (** [locate x] gets the location for [x]. *)
    val locate : t -> Msg.Locator.t
    
    (** [copier x] returns copying function which will preserve
        trap information.
    *)
    val copier : t -> t -> unit

    (** Cleans up all the trapping information. *)
    val cleanup : unit -> unit
	
  end

(** Signature of trapped items. *)
module type Trapped =
  sig

    (** Main type. *)
    type t 

    (** Hashing function. *)
    val hash : t -> int

  end

(** Default wrapper for any type; uses Hashtbl.hash. *)
module Default (X : sig type t end) : Trapped with type t = X.t
      
(** Trap constructor. *)
module Make (X : Trapped) : Sig with type t = X.t

(** String trap --- traps strings to locations. *)
module String : Sig with type t = string
