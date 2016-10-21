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

module type Sig =
  sig
    
    type t
	  
    val attach  : t -> Msg.Locator.t -> unit
    val locate  : t -> Msg.Locator.t

    val copier  : t -> t -> unit
    val cleanup : unit -> unit
	
  end

module type Trapped =
  sig

    type t 

    val hash : t -> int

  end
      
module Default (X : sig type t end) =
  struct
    
    include X

    let hash = Hashtbl.hash

  end

module Make (X : Trapped) =
  struct
    
    type t = X.t
	  
    module XHash = Hashtbl.Make (struct include X let equal = (==) end)
	
    let trapTab = (XHash.create 1024 : Msg.Locator.t XHash.t)
	
    let cleanup () = XHash.clear trapTab
	
    let attach str loc = XHash.add trapTab str loc
	
    let locate str = try XHash.find trapTab str with Not_found -> Msg.Locator.No
	  
    let copier x = 
      let loc = locate x in
      (fun y -> attach y loc)       
	
  end

module String = Make (Default (struct type t = string end))
