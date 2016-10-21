(*
 * Reason: error reasons tree implementation.
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

module Holder =
  struct

    module M = Map.Make (Msg.Locator)
	
    type t = [`Msg of Msg.t | `Comment of string * t] list M.t
    
    let empty = (M.empty : t)

    let add dst msg = 
      let loc = Msg.loc msg in
      let msg = `Msg msg in
      try M.add loc (msg :: M.find loc dst) dst
      with Not_found -> M.add loc [msg] dst

    let merge dst src =
      M.fold 
	(fun loc list dst -> 
	  try
	    M.add loc (list @ M.find loc dst) dst
	  with Not_found -> M.add loc list dst
	) 
	src dst

    let comment str dst = 
      let Some loc =
	M.fold 
	  (fun loc _ acc -> 
	    Some (
	      match acc with
	      | None     -> loc
	      | Some acc -> if Msg.Locator.compare acc loc < 0 then acc else loc
	    )
	  ) 
	  dst 
	  None
      in
      M.add loc [`Comment (str, dst)] empty

    let rec retrieve h limit order =
      let list = M.fold (fun loc list acc -> (loc, list) :: acc) h [] in
      let list = 
	match order with
	| `Acc  -> List.rev list
	| `Desc -> list
      in
      let list =
	match limit with
	| `All     -> list
	| `First n -> 
	    let rec take n = function
	      | []   -> []
	      | h::t -> if n = 0 then [] else h :: (take (n-1) t)
	    in
	    take n list
      in
      List.map 
	(fun (loc, list) ->
	  (
	   loc, 
	   List.map 
	     (function 
	       | `Msg msg -> `Msg msg 
	       | `Comment (str, h) -> `Comment (str, retrieve h limit order)
	     ) list
	  )
	) list

    open Format

    let toString r =
      let module M = Set.Make (String) in
      let buf = Buffer.create 1024 in
      let ppf = formatter_of_buffer buf in
      let rec inner comment list =
	List.iter
	  (fun (loc, list) ->
	    if not comment then fprintf ppf "@[<v 3> Error at %s: " (Msg.Locator.toString loc);
	    ignore 
	      (
	       List.fold_left
		 (fun fence item ->
	   match item with
		   | `Msg msg -> 
		       let s = Msg.toString msg in
		       if M.mem s fence 
		       then fence
		       else (
			 fprintf ppf "@, %s " s;
			 M.add s fence
		       )

		   | `Comment (str, r) -> 
		       fprintf ppf "%s" str;
		       inner true r;
		       fence
		 )
		 M.empty
		 list
	      );
	    if not comment then fprintf ppf "@]@\n"
	  )
	  list
      in
      inner false r;
      pp_print_flush ppf ();
      Buffer.contents buf
      
  end

type p = Holder.t
type retrieved = (Msg.Locator.t * [`Msg of Msg.t | `Comment of string * 'a] list) list as 'a

class t msg =
  object (self : 'a) 

    val tab = Holder.add Holder.empty msg

    method get = tab

    method add      (x   : 'a    ) = {< tab = Holder.merge tab x#get >}
    method comment  (str : string) = {< tab = Holder.comment str tab >}

    method retrieve (l : [`All | `First of int]) (o : [`Acc | `Desc]) = (Holder.retrieve tab l o : retrieved)
    method toString (l : [`All | `First of int]) (o : [`Acc | `Desc]) = (Holder.toString (self#retrieve l o))

  end

let reason msg   = Some (new t msg)
let toString l o = function None -> "no description" | Some x -> x#toString l o

