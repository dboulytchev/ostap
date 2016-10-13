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

    method memoize : 'e . (('self, ('p * 'self, 'e) Types.tag as 'p, 'e) parse -> ('self, 'p, 'e) parse) -> ('self, 'p, 'e) result = 
      fun p -> 
        let getParsedValue : 'e. ((int * int) * int) list -> (('self, ('p * 'self, 'e) Types.tag as 'p, 'e) parse -> ('self, 'p, 'e) parse) -> int -> (('self, 'p, 'e) parse -> ('self, 'p, 'e) parse)  =
         fun t p pos ->
          let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1 in
          let rec find key tab = snd (List.find (fun (k,_) -> equal key k) tab)
            (*match tab with 
            | [] -> raise Not_found
            | (k, v) :: t -> if equal key k 
                             then v
                             else find key t *)
          in Obj.magic (find (Obj.magic p, pos) t)
        in
        let replaceValue : 'e. ((int * int) * int) list -> (('self, ('p * 'self, 'e) Types.tag as 'p, 'e) parse -> ('self, 'p, 'e) parse) -> int -> (('self, 'p, 'e) parse -> ('self, 'p, 'e) parse) -> (((int * int) * int) list) = 
         fun t p pos v ->
          ((Obj.magic p, pos), (Obj.magic v)) :: t 
        in
        let rec increaseBound t p pos =
          let prev = getParsedValue t p this#pos in
          match (p (prev (fun _ -> Failed None))) {< table = t >} with
          | Failed _ -> getParsedValue t p this#pos
          | Parsed ((_, s), _) as parsed -> 
            if s#pos > pos 
            then increaseBound (replaceValue t p this#pos (fun _ -> return parsed)) p s#pos
            else getParsedValue t p this#pos 
        in
        try 
          ((getParsedValue table p this#pos) (fun _ -> Failed None)) this
        with  
          Not_found -> 
            let fail = fun _ -> fail None in
            let newStream = {< table = replaceValue table p this#pos fail >} in
            match (p (fail (fun _ -> Failed None))) newStream  with
            | Failed _ as r -> r
            | Parsed _ as r ->
              let prev = fun _ -> return r in
              let newStream = replaceValue table p this#pos prev in              
              ((increaseBound newStream p this#pos) (fun _ -> Failed None)) this
  end
  
let memo = 
  fun p (s : #memoStream) -> let res = s#memoize p in printf "memoized!\n%!"; res

(*let fix p s = 
  let x' = ref None in  
  let rec fix p s = p (fix p) s in
  let p x s = 
    match !x' with
    | None   -> x' := Some (fun s -> memo x s); p x s 
    | Some x -> p x s 
  in
  fix p s
  *)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
(* class memoStream s = 
  object (this : 'self)
    inherit Matcher.t s
    
    val table : ((int * int) * int) list = []

    method memoize : 'p 'e . (('self, 'p, 'e) parse -> ('self, 'p, 'e) parse) -> ('self, 'p, 'e) result = 
      fun p -> 
        Printf.printf "Table size = %i, p=%d\n" (List.length table) (Obj.magic p);
        let getParsedValue t p pos =
          Printf.printf "Getting value; table size %i\n" (List.length t); 
          let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1 in
          let rec find key tab = 
            match tab with 
            | [] -> (Printf.printf " Not found\n" ; raise Not_found)
            | (k, v) :: t -> if equal key k 
                             then (Printf.printf " Found!\n" ; v)
                             else (Printf.printf " Going deeper\n" ; find key t)
          in
          
          let printTable _ = 
            List.iter (fun ((i1,i2),i3) -> 
              let i_f = match (Obj.magic i3) with 
              | Failed _ -> "fail"
              | Parsed _ -> "succ"
              in 
              Printf.printf "   %i %i %s\n" i1 i2 i_f) t
          in         
          (* printTable (); *)
          Printf.printf "Getting %i for pos %i; TABLE SIZE %i\n %!" (Obj.magic p) pos (List.length t);
          Obj.magic (find (Obj.magic p, pos) t)
        in
        let replaceValue t p pos v =
          let vstr = match v with 
          | Failed _ -> "fail"
          | Parsed _ -> "parsed"
          in
          Printf.printf "Replacing Value %s\n" vstr;
          ((Obj.magic p, pos), (Obj.magic v)) :: t 
        in
        let rec increaseBound t p pos =
          Printf.printf "Increasing bound\n" ;
          match p {< table = t >} with
          | Failed _ -> (Printf.printf "Incr_F\n"; getParsedValue t p this#pos)
          | Parsed ((_, s), _) as parsed -> 
            Printf.printf "s#pos, pos (%i, %i)\n" s#pos pos ;
            if s#pos > pos 
            then increaseBound (replaceValue t p this#pos parsed) p s#pos
            else (Printf.printf "Incr_P\n"; getParsedValue t p this#pos)
        in
        try 
          Printf.printf "Memoize\n";
          getParsedValue table p this#pos           
        with  
          Not_found -> 
            (* left recursion happened *)
            Printf.printf "Not_found\n" ;
            match p {< table = replaceValue table p this#pos (Failed None) >} with
            | Failed _ as r -> Printf.printf "Return failed\n" ; r
            | Parsed ((b, s'), e) as r ->
              Printf.printf "\nParsed value, increasing\n" ;
              increaseBound (replaceValue table p this#pos r) p this#pos
  end
  
let memo = 
  fun p (s : #memoStream) -> s#memoize p
*)
(*let rec fix' p s = p (fix' p) s *)

(* let rec genious_fix p s = 
  let gfixp = genious_fix p in 
  let gfixp_hack s = 
    printf "calling gfixp_hack on pos %d\n%!" s#pos;
    gfixp s
  in
  printf "gfixp = %d\n%!" (Obj.magic gfixp);
  p gfixp_hack s

let fix _p = 
  printf "Combinators.fix\n%!";
  let x' = ref None in  
  let p x s = 
    printf "p receives x = (gfix p) = %d \n%!" (Obj.magic x);
    match !x' with
    | None   -> 
       printf "Settting flag\n";
       x' := Some (fun s -> memo x s);        
       printf "calling _p=%d on x=%d on pos %d\n%!" (Obj.magic _p) (Obj.magic x) s#pos;
       _p x s 
    | Some x -> _p x s 
  in
  (fun s -> 
     printf "Inside `fix` on pos %d\n%!" s#pos;
     (genious_fix p) s)
   *)
  (* let y' = ref None in
  let whatever x s =
    match !y' with
    | None -> y' := Some (fix p); _p x s
    | Some x -> _p x s
  in
  _p (whatever (fix p)) s
  *)
  (*_p(_p(_p(_p(_p (_p (fix p)))))) s*)

(* let fix p =
    let f = ref None
    in
    let rec fix p s =
        match !f with
        | None -> 
            f := Some (memo (fix p));
            let (Some g) = !f 
            in
            p g s
        | Some x -> p x s
    in
    fun s -> p (fix p s) s
*)
(*
let fix _p s = 
  let rec fix p s = 
    let f s = memo (fix _p) s
    in
    _p f s in
  _p (fix _p) s
*)
(*let fix _p s = 
  let x' = ref None in     
  let rec fix p s =
    let p' s = fix _p s in
    match !x' with
    | None   -> x' := Some (fun s -> memo p' s); p p' s 
    | Some x -> p x s
  in
  _p (fix _p) s
  *)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
