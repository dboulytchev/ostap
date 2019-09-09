(*
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
  let id   x = x                in
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
  let buf  = Bytes.make len ' ' in
  really_input inch buf 0 len;
  close_in inch;
  Bytes.unsafe_to_string buf

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
      object(self : 'self)
	inherit checkKeywords keywords
	method virtual get      : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
  method private getIdent : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
   fun k -> self#get name regexp
       (fun t s ->
          let r = Token.repr t in
          if self#keyword r
          then Types.failWith (new Reason.t (Msg.make "%0 expected" [|name|] (Token.loc t)))
          else k r s)
      end

    class virtual uident keywords s =
      object inherit genericIdent "[A-Z]\([a-zA-Z_0-9]\)*\\b" "u-identifier" keywords s as ident
	method getUIDENT : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result = ident#getIdent
      end

    class virtual lident keywords s =
      object inherit genericIdent "[a-z]\([a-zA-Z_0-9]\)*\\b" "l-identifier" keywords s as ident
	method getLIDENT : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result = ident#getIdent
      end

    class virtual ident keywords s =
      object inherit genericIdent "[a-zA-Z]\([a-zA-Z_0-9]\)*\\b" "identifier" keywords s as ident
	method getIDENT : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result = ident#getIdent
      end

    class virtual decimal s =
      let regexp = Re_str.regexp "-?[0-9]+" in
      object(self : 'self)
        method virtual get : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
        method getDECIMAL  : 'b. (int -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
          fun k -> self#get "decimal constant" regexp (fun t s -> k (int_of_string (Token.repr t)) s)
      end

    class virtual string s =
      let regexp = Re_str.regexp (*"\"\([^\"]\|\\\"\)*\""*) "\"[^\"]*\"" in
      object(self : 'self)
        method virtual get : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
        method getSTRING   : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
          fun k -> self#get "decimal constant" regexp (fun t s -> k (Token.repr t) s)
      end

    class virtual char s =
      let regexp = Re_str.regexp "'\([^']\|\\'\)'" in
      object(self : 'self)
        method virtual get : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
        method getCHAR     : 'b. (Char.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
          fun k -> self#get "character constant" regexp (fun t s -> k ((Token.repr t).[1]) s)
      end

    class skip skippers s =
      object inherit Matcher.t s
	val skipper = Skip.create skippers s
	method skip = skipper
      end

  end

  let parse l p =
    Combinators.unwrap (p l (fun res s -> Types.Parsed ((res, s), None)))
      (fun x -> `Ok x)
      (fun x ->
         match x with
         | Some err ->
           let [loc, m :: _] = err#retrieve (`First 1) (`Desc) in
           let m =  match m with `Msg m -> m | `Comment (s, _) -> Msg.make s [||] loc in
           `Fail (Msg.toString m)
         | None -> `Fail "Oh, fuck"
      )
