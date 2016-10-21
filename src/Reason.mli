(*
 * Reason: error reasons tree interface.
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

(** Predefined implementation of reason type to describe various errors happened 
    during parsing. 
*)

(** Interior type. *)
type p

(** Exterior representation *)
type retrieved = (Msg.Locator.t * [`Msg of Msg.t | `Comment of string * 'a] list) list as 'a

(** Main class. Keeps track on error reasons and comments. Takes an ordinary message on creation. *)
class t :
  Msg.t ->
  object ('a)
    method add      : 'a -> 'a                                              (** Add another reason                 *)
    method comment  : string -> 'a                                          (** Comment reason(s)                  *)
    method get      : p                                                     (** Interior friend function           *)
    method retrieve : [`All | `First of int] -> [`Acc | `Desc] -> retrieved (** Get reason in public form          *)
    method toString : [`All | `First of int] -> [`Acc | `Desc] -> string    (** Retrieve and apply default printer *)
  end

(** Synonym for reason construction. *)
val reason : Msg.t -> t option

(** Standard printer for optional reason. The first two arguments have the following meanings: 

    {ul {- [ `All] makes printer show {i all} the messages while [ `First n] asks only for [n] first of them;
         this is desirable since there can potentially be many parasite messages;}
      {- [ `Acc] asks printer to output messages in {i ascending} order w.r.t. locator information while
         [ `Desc] asks to print them in {i descending} order.}
    }
  *)
val toString : [`All | `First of int] -> [`Acc | `Desc] -> t option -> string
