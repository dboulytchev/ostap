1(*
 * Util: predefined Ostap utilities.
 * Copyright (C) 2006-2009
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

open Combinators
open Matcher 
open Printf 

module Ostap =
  struct

    module Combinators = Combinators

  end

ostap (
  keyword[name]: @(name ^ "\\b" : name)
)

let (~$) = keyword

ostap (
  listByWith[delim][item][f][x]: h:item result:(-delim item) * with{f x h}{f} {result}
)

ostap (
  listBy[delim][item]: h:item t:(-delim item)* {h::t}
)

ostap (
  listWith[item][f][x]: listByWith[ostap (",")][item][f][x]
)

ostap (
  list: listBy[ostap (",")]
)

ostap (
  list0ByWith[delim][item][f][x]: h:item result:(-delim item) * with{f x h}{f} {result} | empty {x}
)

ostap (
  list0By[delim][item]: listBy[delim][item] | empty {[]}
)

ostap (
  list0With[item][f][x]: list0ByWith[ostap (",")][item][f][x]
)

ostap (
  list0: list0By[ostap (",")]
)

let left  f c x y = f (c x) y
let right f c x y = c (f x y)

ostap (
  id[x]: x
)

let expr f ops opnd =
  let ops =
    Array.map 
      (fun (assoc, list) ->
        let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in
        assoc = `Nona, altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list)
      )
      ops 
  in
  let n      = Array.length ops in
  let op   i = snd ops.(i)      in
  let nona i = fst ops.(i)      in
  let id x   = x                in  
  let ostap (
    inner[l][c]: f[ostap (
      {n = l                } => x:opnd {c x}
    | {n > l && not (nona l)} => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? {
        match b with None -> c x | Some x -> x
      }
    | {n > l && nona l} => x:inner[l+1][id] b:(op[l] inner[l+1][id])? {
        c (match b with None -> x | Some (o, y) -> o id x y)
      })]
  )
  in 
  ostap (inner[0][id])

let read name = 
  let inch = open_in_bin name in
  let len  = in_channel_length inch in
  let buf  = String.make len ' ' in
  really_input inch buf 0 len;
  close_in inch;
  buf

module Lexers =
  struct

    let isKeyword keywords = 
      let module S = Set.Make (String) in
      let s = List.fold_left (fun s k -> S.add k s) S.empty keywords in
      (fun i -> S.mem i s)

    class checkKeywords keywords =
      let k = isKeyword keywords in
      object
	method private keyword = k
      end

    class virtual genericIdent regexp name keywords s =
      let regexp = Re_str.regexp regexp in 
      object(self : 'a)
	inherit checkKeywords keywords
	method virtual get      : string -> Re_str.regexp -> ('a, Token.t, Reason.t) Types.result
        method private getIdent : ('a, string, Reason.t) Types.result = 
	  Types.bind 
	    (self#get name regexp) 
	    (fun t -> 
	       let r = Token.repr t in
	       if self#keyword r then `Fail (new Reason.t (Msg.make "%0 expected" [|name|] (Token.loc t)))
	       else `Ok r
	    )
      end
   
    class virtual uident keywords s = 
      object inherit genericIdent "[A-Z]\([a-zA-Z_0-9]\)*\\b" "u-identifier" keywords s as ident
	method getUIDENT = ident#getIdent
      end

    class virtual lident keywords s = 
      object inherit genericIdent "[a-z]\([a-zA-Z_0-9]\)*\\b" "l-identifier" keywords s as ident
	method getLIDENT = ident#getIdent
      end

    class virtual ident keywords s =
      object inherit genericIdent "[a-zA-Z]\([a-zA-Z_0-9]\)*\\b" "identifier" keywords s as ident
	method getIDENT = ident#getIdent
      end

    class virtual decimal s =
      let regexp = Re_str.regexp "-?[0-9]+" in
      object(self : 'a)
	method virtual get : string -> Re_str.regexp -> ('a, Token.t, Reason.t) Types.result
	method getDECIMAL : ('a, int, Reason.t) Types.result = 
	  Types.bind 
	    (self#get "decimal constant" regexp)
	    (fun t -> `Ok (int_of_string @@ Token.repr t))
      end

    class virtual string s =
      let regexp = Re_str.regexp "" in
      object(self : 'a)
	method virtual get : String.t -> Re_str.regexp -> ('a, Token.t, Reason.t) Types.result
	method getSTRING : ('a, String.t, Reason.t) Types.result =
	  Types.bind
	    (self#get "string constant" regexp)
	    (fun t -> `Ok (Token.repr t))
      end

    class skip skippers s =
      object inherit t s
	val skipper = Skip.create skippers
	method skip = skipper s
      end

  end

let parse l p =
  Combinators.unwrap (p l) 
    (fun x -> `Ok x) 
    (fun (Some err) ->
       let [loc, m :: _] = err#retrieve (`First 1) (`Desc) in
       let m =  match m with `Msg m -> m | `Comment (s, _) -> Msg.make s [||] loc in
       `Fail (Msg.toString m)
    )
