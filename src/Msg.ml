(*
 * Msg: parsing message module.
 * Copyright (C) 2006
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

open Printf

module Coord =
   struct

      type t = int * int

      let line = fst
      let col  = snd

      let toString (r, c) = sprintf "(%d:%d)" r c

      let next isNewline (r, c) = if isNewline then (r + 1, 1) else (r, c + 1)

      let shift coord s b n =
         let rec inner i coord =
            if i = n 
            then coord
            else inner (i+1) (next (s.[i] = '\n') coord)
         in inner b coord

      let compare (r, c) (r', c') =
         let x = compare r r' in
         if x = 0 then compare c c' else x

   end

module MC = Map.Make(Coord)

module rec Locator :
   sig

     type t =
         No
       | Point    of Coord.t
       | Interval of Coord.t * Coord.t
       | Set      of t list

     val makeInterval : t -> t -> t
     val least  : t -> Coord.t
     val most   : t -> Coord.t
     val updateToString : FileLoc.r -> string -> unit
     val toString : t -> string
     val compare : t -> t -> int

   end
   =
   struct

      type t = No | Point of Coord.t | Interval of Coord.t * Coord.t | Set of t list
      and  l = t

      let makeInterval x y =
         match x, y with
         | Point x, Point y -> Interval (x, y)
         | _ -> Set [x; y]

      let relocs = ref MC.empty
      let source = ref ""
      let defaultWriter _ coord = !relocs, None, Coord.toString coord
      let writer = ref defaultWriter

      let rec least = function
         | No              -> (0, 0)
         | Point     x
         | Interval (x, _) -> x
         | Set x -> List.hd (List.sort Coord.compare (List.map least x))

      let rec most = function
         | No              -> (0, 0)
         | Point     x
         | Interval (_, x) -> x
         | Set x -> List.hd (List.sort (fun x y -> - Coord.compare x y) (List.map most x))

      let updateToString rlcs src =
         if MC.is_empty rlcs || src = ""
         then begin
            relocs := MC.empty;
            source := "";
            writer := defaultWriter
         end
         else begin
            relocs := FileLoc.addFirst rlcs;
            source := src;
            writer := fun rlcs coord -> let succ, fil, coord = FileLoc.getSuccReloc !source rlcs coord in succ, fil, Coord.toString coord
         end

      let rec toString = function
         | No -> ""
         | Point x ->
            let _, fil, coord = !writer !relocs x in
            (match fil with None -> "" | Some fil -> sprintf "%s: " fil) ^ coord
         | Interval (x, y) ->
            let succ, filx, x =
               !writer !relocs x in
            let    _, fily, y =
               !writer succ    y in
            (match filx, fily with
            | None, None -> sprintf "%s-%s" x y
            | Some filx, None -> sprintf "(%s: %s)-%s" filx x y
            | None, Some fily -> sprintf "%s-(%s: %s)" x fily y
            | Some filx, Some fily ->
               if filx = fily
               then sprintf "%s: %s-%s" filx x y
               else sprintf "(%s: %s)-(%s: %s)" filx x fily y
            )
         | Set x ->
            let module M = View.List (struct type t = l let toString = toString end) in
            M.toString x

      let compare x y =
         if Pervasives.compare x y = 0 then 0
         else
         match (x, y) with
         | No, No -> 0
         | No, _  -> -1
         | _ , No -> 1
         | _      -> Coord.compare (least x) (least y)

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
   =
   struct

      open Locator

      type t = string * Locator.t
      type r = (int * (string * Coord.t)) list MC.t

      let no = "", No
      let filename = ref ""
      let debug  = ref false

      let interval x y = !filename, makeInterval x#loc y#loc

      let toText (fil, loc) = sprintf "at %s in file %s" (toString loc) fil

      let brackLoc loc = if loc = No then "" else sprintf "[%s]" (toString loc)

      let unite (fnx, x) (fny, y) =
         if fnx = fny
         then (fnx,
         (match (x, y) with
         | No, x
         | x, No -> x
         | x, y -> Interval (least x, most y)
         ))
         else (sprintf "%s%s, %s%s" fnx (brackLoc x) fny (brackLoc y), No)

      let toLineDir (fil, loc) s = sprintf "\n#line \"%s\" %s\n%s\n#line \"%s\" %s\n" fil (Coord.toString (least loc)) s fil (Coord.toString (most loc))

      let splitSucc c m =
         let prev, this, succ = MC.split c m in
         let (key, bnd) as res =
            match this with
            | Some item -> c, item
            | None -> MC.max_binding prev
         in
         res, MC.add key bnd succ

      let shift s i loc_from loc_to reloc =
         let rec inner i loc reloc =
            if Coord.compare loc loc_to = 0
            then reloc
            else let next = Coord.next (s.[i] = '\n') in
                 inner (i+1) (next loc) (next reloc)
         in inner i loc_from reloc

      let getSuccReloc s m p =
         let (loc, relocs), succ = splitSucc p m in
         let (pos, (fil, reloc)) = List.hd relocs in
         let reloc = shift s pos loc p reloc in
         succ, Some fil, reloc

      let stripLines s =
         let r = Re_str.regexp "\r?\n#line \"\([^\"]*\)\" (\([0-9]+\):\([0-9]+\))\r?\n" in
         let makeInt i s = int_of_string (Re_str.matched_group i s) in
         let rec inner pos loc m s acc =
            try
               if !debug then printf "loc was: %s\n" (Coord.toString loc);
               let first = Re_str.search_forward r s 0 in
               let reloc = (Re_str.matched_group 1 s, (makeInt 2 s, makeInt 3 s)) in
               let loc = if first > 0 then Coord.shift loc s 0 first else loc in
               let current = try MC.find loc m with Not_found -> [] in
               let last = Re_str.match_end () in
               let newpos = pos + first in
               if !debug then begin
                  printf "loc is: %s\n" (Coord.toString loc);
                  printf "'";
                  for i = 0 to min 20 (String.length s - 1) do printf "%c" s.[i] done;
                  printf "'\n";
               end;
               inner newpos loc (MC.add loc ((newpos, reloc)::current) m) (Re_str.string_after s last) (acc ^ (Re_str.string_before s first))
            with Not_found -> m, acc ^ s
         in inner 0 (1, 1) MC.empty s ""

      let addFirst m = MC.add (0, 0) [0, ("", (0, 0))] m

      let printRelocs m =
         let module VL = View.List (View.Pair(View.Integer)(View.Pair(View.String)(Coord))) in
         MC.iter (fun p lst -> printf "%s: %s\n" (Coord.toString p) (VL.toString lst)) m

      let printReloc s m (Interval (p, q) as intrvl) =
         let succ, Some fil, beg_c = getSuccReloc s m p in
         let _, _, end_c = getSuccReloc s succ q in
         printf "%s -> \"%s\" %s\n" (toString intrvl) fil (toString (Interval (beg_c, end_c)))

   end

type t = {phrase: string; args: string array; loc: Locator.t} 

let make      phrase args loc = {phrase=phrase; args=args; loc=loc}
let loc       t               = t.loc

let phrase    phrase          = make phrase [||] Locator.No
let orphan    phrase args     = make phrase args Locator.No

let string t = 
  let parmExpr = Re_str.regexp "%\\([0-9]+\\)" in
  Re_str.global_substitute 
    parmExpr  
    (fun s -> 
      try 
        t.args.(int_of_string (Re_str.replace_matched "\\1" s))
      with
      | Failure "int_of_string" -> 
          raise (Failure 
                   (sprintf "invalid integer parameter specification in message phrase \"%s\"" s)
                )
	    
      | Invalid_argument "index out of bounds" ->
          raise (Failure 
                   (sprintf "index out of bound while accessing message parameter in \"%s\"" s)
                )
    )
    t.phrase
    
let toString t =
  let message = string t in
    match Locator.toString t.loc with
    | ""  -> message
    | loc -> message ^ " at " ^ loc
      
let augment msg loc = match msg.loc with Locator.No -> {msg with loc = loc} | _ -> msg
let augmentList msgs loc = List.map (fun x -> augment x loc) msgs

let extend msg str = {msg with phrase=str ^ msg.phrase}
let extendList msgs str = List.map (fun msg -> extend msg str) msgs
