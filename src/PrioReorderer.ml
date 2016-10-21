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

module type Expression =
  sig

    type t              
   
    val discover : t -> [`Infix of int * t * t | `Other]
    val replace  : t -> t -> t -> t
    val map      : (t -> t) -> t -> t
	  
  end
      
module Make (E : Expression) = 
  struct
      
    let rec sort expr =
      let rec reduce p ((oper, opnd) as stacks) = 
        match oper with
        | (t, p') :: oper' when (p <= p') ->
            let r::l::tl = opnd in
            reduce p (oper', (E.replace t l r) :: tl)
        | _ -> stacks
      in
      let rec putin t ((oper, opnd) as stacks) =
	match E.discover t with
	| `Infix (p, l, r) ->
            let oper', opnd' = reduce p (putin l stacks) in
            putin r ((t, p) :: oper', opnd')
	| `Other -> (oper, (E.map sort t) :: opnd)
      in 
      let _, result::_ = reduce (-1) (putin expr ([], [])) in
      result    

  end
    























