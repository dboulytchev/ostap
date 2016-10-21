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

type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option

type ('a, 'b, 'c) result = ('b * 'a, 'c) tag
and  ('a, 'b, 'c) parse  = 'a -> ('a, 'b, 'c) result

let bind result f = 
  match result with
  | Parsed ((v, s), err) -> 
      (match f v with
       | `Ok v'     -> Parsed ((v', s), err)
       | `Fail err' -> Failed (Some err')
      )
  | Failed x -> Failed x
