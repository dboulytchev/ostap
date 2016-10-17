(*
 * Ostap: basic set of parser combinators.
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

open Printf
open Matcher
open List
open Types

let join = function
  | None   -> fun y -> y
  | Some x -> function None -> Some x | Some y -> Some (x#add y)

let comment str = function
  | None   -> None
  | Some m -> Some (m#comment str)

let return x = (fun s -> Parsed ((x, s), None))
let cast     = function Failed x -> Failed x | _ -> invalid_arg "Ostap.cast"

let map f p s = 
  match p s with
  | Parsed ((b, s'), e) -> Parsed ((f b, s'), e)
  | x -> cast x

let (-->) p f = map f p

let empty s   = Parsed (((), s), None)
let fail  r s = Failed r
let lift  s   = Parsed ((s, s), None)
let sink  p s = 
  match p s with
  | Parsed ((s, _), f) -> Parsed ((s, s), f)
  | Failed x           -> Failed x

let alt x y s =
  match x s with 
  | Failed x ->      
      (match y s with 
      | Failed y -> Failed (join x y) 
      | Parsed (ok, err) -> Parsed (ok, join x err)
      )     
  | x -> x
    
let (<|>) = alt

let seq x y s =
  match x s with
  | Parsed ((b, s'), err) ->
      (match y b s' with 
      | Failed  x     -> Failed (join err x) 
      | Parsed (s, e) -> Parsed (s, join err e)
      )
  | x -> cast x
    
let (|>) = seq

let opt p s =
  match p s with 
  | Parsed ((x, s'), d) -> Parsed ((Some x, s'), d) 
  | Failed d            -> Parsed ((None, s), d)

let (<?>) = opt

let manyFold f init p =
  let rec inner err acc s =
    match p s with
    | Parsed ((x, s'), d) -> inner (join err d) (f acc x) s'
    | Failed d            -> Parsed ((acc, s), join err d)
  in
  inner None init 

let many p = 
  (manyFold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

let (<*>) = many

let someFold f init p = p |> (fun h -> manyFold f (f init h) p)

let some p = (someFold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

let (<+>) = some
    
let guard p f r s = 
  match p s with
  | (Parsed ((b, _), _) as x) -> 
      if f b 
      then x 
      else Failed (match r with None -> None | Some r -> Some (r b))
  | y -> y

let comment p str s =
  match p s with
  | (Parsed _ as x) -> x
  | Failed m -> Failed (comment str m)

let altl l = List.fold_left (<|>) (fail None) l

let unwrap r f g =
  match r with
  | Parsed ((x, _), _) -> f x
  | Failed x           -> g x
  
class memoStream s = 
  object (this : 'self)
    inherit Matcher.t s
    
    val table : ((int * int) * int) list = []

    method memoize : 'p 'e . ('self, 'p, 'e) parse -> ('self, 'p, 'e) result = 
      fun p -> 
        let getParsedValue t p pos =
          let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1 in
          let rec find key tab = 
            match tab with 
            | [] -> raise Not_found
            | (k, v) :: t -> if equal key k 
                             then v
                             else find key t
          in
          Obj.magic (find (Obj.magic p, pos) t)
        in
        let replaceValue t p pos v =
          ((Obj.magic p, pos), (Obj.magic v)) :: t 
        in
        let rec increaseBound t p pos =
          match p {< table = t >} with
          | Failed _ -> getParsedValue t p this#pos
          | Parsed ((_, s), _) as parsed -> 
            if s#pos > pos 
            then increaseBound (replaceValue t p this#pos parsed) p s#pos
            else getParsedValue t p this#pos
        in
        try 
          getParsedValue table p this#pos
        with  
          Not_found -> 
            match p {< table = replaceValue table p this#pos (Failed None) >} with
            | Failed _ as r -> r
            | Parsed ((b, s'), e) as r ->
              increaseBound (replaceValue table p this#pos r) p this#pos
  end
  
let memo = 
  fun p (s : #memoStream) -> s#memoize p

let fix p s = 
  let x' = ref None in  
  let rec fix p s = p (fix p) s in
  let help p prev = 
    match p with 
    | Failed _ -> prev
    | Parsed ((_,s),_) -> 
      match prev with 
      | Parsed ((_,prevS),_) -> if s#pos < prevS#pos then prev else p
      | _ -> p
  in
  let p x s = 
    match !x' with
    | None   -> x' := Some (fun s -> memo x s); help (p x s) (x s)
    | Some x -> help (p x s) (x s)
  in
  fix p s

