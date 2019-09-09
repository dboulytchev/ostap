(*
 * Types: common types.
 * Copyright (C) 2016
 * Ekaterina Verbitskaja, St.Petersburg State University
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

 type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option | Empty
 type ('stream, 'b, 'c) result = ('b * 'stream, 'c) tag

let emptyResult = Failed None
let failWith x = Failed (Some x)

 module K :
   sig
     type ('a, 'stream, 'b, 'c) t = 'a -> 'stream -> ('stream, 'b, 'c) result
     type ks

     val singleton : ('a, 'stream, 'b, 'c) t -> ks
     val add       : ('a, 'stream, 'b, 'c) t -> ks -> ks
     val fold      : (('a, 'stream, 'b, 'c) t -> ('stream, 'b, 'c) result -> ('stream, 'b, 'c) result) -> ks -> ('stream, 'b, 'c) result -> ('stream, 'b, 'c) result
     val empty     : ks
     val length    : ks -> int

   end =
   struct

     type ('a, 'stream, 'b, 'c) t = 'a -> 'stream -> ('stream, 'b, 'c) result

     module Ks = Set.Make (
       struct
 	type t = Obj.t

 	let compare x y = (Pervasives.compare : int -> int -> int) (Obj.magic x) (Obj.magic y)
       end
     )
     type ks = Ks.t

     let singleton k         = Ks.add (Obj.repr k) Ks.empty
     let add       k ks      = Ks.add (Obj.repr k) ks
     let fold      f ks acc  = Ks.fold (fun k acc -> f (Obj.magic k) acc) ks acc
     let empty               = Ks.empty
     let length      ks      = Ks.cardinal ks
   end

 type ('a, 'stream, 'b, 'c) k       = ('a, 'stream, 'b, 'c) K.t
 type ('a, 'stream, 'b, 'c) parserr  = 'stream -> ('a, 'stream, 'b, 'c) k -> ('stream, 'b, 'c) result
 type ('a, 'stream, 'b, 'c) parser' =            ('a, 'stream, 'b, 'c) k -> ('stream, 'b, 'c) result

let bind p k f =
  p (fun a' s' ->
       match k a' s' with
       | Parsed ((v, s), err) ->
           (match f v with
            | `Ok v'     -> Parsed ((v', s), err)
            | `Fail err' -> Failed (Some err')
           )
       | Failed x -> Failed x)
