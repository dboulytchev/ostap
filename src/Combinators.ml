open Lazy
open Matcher
open Types
open Reason

let memo_k =
  fun k ->
    let table : ('a * 'stream, ('stream, 'b, 'c) result) Hashtbl.t = Hashtbl.create 16 in
    fun a s ->
      match (Hashtbl.fold
              (fun (a', s') r' acc ->
                 match acc with
                 | Some _ -> acc
                 | None when ((Pervasives.compare : int -> int -> int) (Obj.magic a) (Obj.magic a') == 0) && (s # equal s') -> Some r'
                 | _ -> None)
              table None) with
      | None ->
        let r = k a s in
        (match r with
        | Empty -> r
        | _ -> Hashtbl.add table (a, s) r; r)
      | Some r -> r

let join = function
| None   -> fun y -> y
| Some x -> function None -> Some x | Some y -> Some (x#add y)

let comment str = function
| None   -> None
| Some m -> Some (m#comment str)

let return =
  fun x s k -> k x s

let cast =
  function Failed x -> Failed x | _ -> invalid_arg "Ostap.cast"

let map =
  fun f p s k ->
    p s (memo_k (fun a s -> k (f a) s))

let (-->) p f = map f p

let empty =
  fun s k -> return () s k

let fail =
 fun r s k -> Failed r

let lift =
  fun s k -> k s s

let sink =
  fun p s k ->
    match p s k with
    | Parsed ((s, _), f) -> Parsed ((s, s), f)
    | Failed x           -> Failed x

let memoresult =
  fun p ->
    let ss : ('stream * 'a) list ref = ref      [] in
    let ks :                K.ks ref = ref K.empty in
    fun k ->
      if K.length !ks = 0
      then (
        ks := K.singleton k;
        p (memo_k (fun a s ->
            match List.find_all (fun (s', a') -> ((Pervasives.compare : int -> int -> int) (Obj.magic a) (Obj.magic a') == 0) && (s # equal s')) !ss with
             | [] -> (ss := (s, a) :: !ss;
                      K.fold (fun k acc -> acc <@> (k a s)) !ks (Empty))
             |  _ -> Empty
          )))
      else
        (ks := K.add k !ks;
         List.fold_left (fun acc x -> match acc, x with
                                      | Parsed _, _           -> acc
                                      | Failed _, Parsed _    -> x
                                      | Failed _, Failed opt2 -> Failed (opt2)
                                      | Empty, _              -> x
                                      | _, Empty              -> acc)
                        Empty
                        (List.map (fun (s, a) -> (k a s)) !ss))

let memo =
  fun f ->
    let table : ('stream, ('a, 'stream, 'b, 'c) parser') Hashtbl.t = Hashtbl.create 16 in
    fun s k ->
      match (Hashtbl.fold (fun s' p' acc -> match acc with
                                            | Some _                   -> acc
                                            | None when (s # equal s') -> Some p'
                                            | _                        -> None
                          ) table None) with
      | None -> let r = memoresult @@ (f s) in
                  Hashtbl.add table s r; r k
      | Some x -> x k

let alt =
  fun x y -> memo (fun s k -> let res1 = (x s k) in
                    match res1 with
                    | Parsed _ -> res1
                    | _ -> res1 <@> (y s k))
  (* fun x y -> memo (fun s k -> (x s k) <@> (y s k)) *)

let (<|>) = alt

let seq =
  fun x y s k -> x s (memo_k (fun a s' -> y a s' k))

let (|>) = seq

let opt =
  fun p ->
    memo (fun s k -> let s' = Oo.copy s in
                     let k' = memo_k (fun a s -> k (Some a) s) in
                     (p s k') <@> (k None s'))

let (<?>) = opt

let rec manyFold =
  fun f init p s k -> (empty |> fun _ -> return init) s k <@>
                      (p                 |> (fun xp  ->
                       manyFold f init p |> (fun xps ->
                       return (f xp xps)))) s k
(*
let rec many : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p s k ->
    let result : ('stream, 'b, 'c) result ref = ref (k [] s) in
    let rec loop alist stream =
      p stream (memo_k (fun a stream' ->
                  let alist' = List.rev (a :: (List.rev alist)) in
                  let curResult = k (alist') stream' in
                  result := curResult <@> !result;
                  let tmp = loop (alist') stream' in
                  curResult))
    in
    let tmp = loop [] s in
    !result *)

let rec many : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p -> memo (fun s k ->
    let rec loop alist stream result =
      result <@>
      p stream (memo_k (fun a stream' ->
                  let alist' = alist @ [a] in
                  let curResult = k (alist') stream' in
                  loop (alist') stream' (curResult <@> result)))
  in
  loop [] s (k [] (Oo.copy s)))
(*
let many : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p s k -> manyFold (fun xp xps -> xp :: xps) [] p s k
 *)
let (<*>) = many

let someFold =
  fun f init p -> p                 |> (fun xp  ->
                  manyFold f init p |> (fun xps ->
                  return (f xp xps)))

(* let some : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p -> p        |> (fun x  ->
          (many p)  |> (fun xs ->
          return (x :: xs)))
*)
let rec some : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p -> memo (fun s k ->
    let rec loop alist stream =
      p stream (memo_k (fun a stream' ->
                  let alist' = alist @ [a] in
                  let curResult = k (alist') stream' in
                  curResult <@> loop (alist') stream'))
  in
  loop [] s)

let (<+>) = some

let guard =
  fun p f r s k ->
    p s (memo_k (fun a s -> if f a
                            then k a s
                            else Failed (match r with
                                         | None -> None
                                         | Some r -> Some (r a))))

let unwrap r f g =
match r with
| Parsed ((x, _), _) -> f x
| Failed x           -> g x

let altl =
  fun l -> List.fold_left (<|>) (fail None) l

let comment p str s k =
  match p s k with
  | (Parsed _ as x) -> x
  | Failed m -> Failed (comment str m)

let fix =
  fun f -> let rec p = lazy (f (fun t -> force p t)) in force p

let fixPoly =
  fun l -> fix (fun self l -> Array.map (fun li x -> li (self l) x) l) l
