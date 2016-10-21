(*
 * Msg: parsing message module.
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

(** Parsing message interface. *)

(** {2 Messaging operations} *)

(** Text coordinate. *)
module Coord :
  sig

    (** Type synonym: line, column. *)  
    type t = int * int

    (** Gets line of coordinate. *)
    val line : t -> int

    (** Gets column of coordinate. *)
    val col : t -> int

    (** String conversion. *)
    val toString : t -> string
    
    (** [next isNewLine loc] gets next coord depending on the current symbol *)
    val next : bool -> t -> t
    
(** [shift loc s b e] takes text coordinates [loc], string [s] and two indexes [b] and [e], 
    scans [s] from [b] to [e] inclusively and shifts [loc] to take newlines into account.
*)
    val shift : t -> string -> int -> int -> t

    (** Comparison function. *)
    val compare : t -> t -> int

  end

module MC : Map.S with type key = Coord.t

(** Various ways to denote the location in the source text. *)
module rec Locator :
  sig

    (** Locator type. *)
    type t =
        No                             (** No location defined             *)
      | Point    of Coord.t            (** One point in the text           *)
      | Interval of Coord.t * Coord.t  (** Contiguous interval of points   *)
      | Set      of t list             (** Non-contiguous set of locations *)

    (** Makes simple interval of two points or set of two non-point locators. *)
    val makeInterval : t -> t -> t

    val least  : t -> Coord.t
    val most   : t -> Coord.t
    val updateToString : FileLoc.r -> string -> unit

    (** String conversion. *)
    val toString : t -> string

    (** Comparison function. *)
    val compare : t -> t -> int

  end
and FileLoc :
  sig

    type t = string * Locator.t
    type r = (int * (string * Coord.t)) list MC.t

    val no           : t
    val filename     : string ref
    val debug        : bool ref
    val interval     : <loc: Locator.t; ..> -> <loc: Locator.t; ..> -> t
    val toText       : t -> string
    val unite        : t -> t -> t
    val toLineDir    : t -> string -> string
    val getSuccReloc : string -> r -> Coord.t -> r * string option * Coord.t
    val stripLines   : string -> r * string
    val addFirst     : r -> r
    val printRelocs  : r -> unit
    (** works only before calling Locator.updateToString *)
    val printReloc   : string -> r -> Locator.t -> unit

  end

(** Type of the message. *)
type t

(** General constructor. [make phrase args loc] creates message with format string [phrase],
    arguments [args] and locator [loc]. Format string may contain parameter references of the
    form ["%0"], ["%1"], ["%2"] etc. These references to be substituted with corresponding actual
    values (i.e. [args.(0)], [args.(1)] etc.) during toString visualization. For example,
    [toString {phrase="%0 %1 not found"; args=[|"type"; "int"|]; loc=No}] is
    ["type int not found"]. *) 
val make : string -> string array -> Locator.t -> t

(** Custom constructor; takes no parameters, no locator. *)
val phrase : string -> t

(** Custom constructor; takes no locator. *)
val orphan : string -> string array -> t

(** Gets locator of a message. *)
val loc : t -> Locator.t

(** Substitute parameters. *)
val string : t -> string

(** Visualization with parameter substitution. *)
val toString : t -> string

(** Augment the message with the location (replaces [Locator.No] (if any) in its argument 
    with the [loc]). 
  *)
val augment : t -> Locator.t -> t

(** Augment the list of messages with the location. *)
val augmentList : t list -> Locator.t -> t list

(** Extends phrase of the message by given string. *)
val extend : t -> string -> t

(** Extends all phrases of the list of messages by given string. *)
val extendList : t list -> string -> t list

