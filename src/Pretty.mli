(*
 * Pretty: basic set of pretty-printing combinators.
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

(** Pretty --- a general set of pretty-printer combinators. Implemented as a
    wrapper for the standard module {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}
    to ease its use.
  *)

(** Printer type. *)
type printer = Format.formatter -> unit

(** String conversion. *)
val toString : printer -> string

(** Empty printer. *)
val empty : printer

(** Newline printer. *)
val newline : printer

(** Break printer (prints a logical break, see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). *)
val break : printer

(** Opens box (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
 *)
val box : printer

(** Opens vertical box (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val vbox : printer

(** Opens horizontal box (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val hbox : printer

(** Opens horizontal/vertical box (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val hovbox : printer

(** Opens horizontal and vertical box (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val hvbox : printer

(** Closes box (see standard module
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val endbox : printer

(** [string str] makes printer which prints [str]. *)
val string : string -> printer

(** [int n] makes printer of int. *)
val int : int -> printer

(** [char c] makes printer of char. *)
val char : char -> printer

(** [bool b] makes printer of bool. *)
val bool : bool -> printer

(** [float f] makes printer of float. *)
val float : float -> printer

(** List printing combinator. *)
val seq : printer list -> printer

(** Array printing combinator. *)
val seqa : printer array -> printer

(** [listBy del list] prints elements of [list] delimited by [del]. *)
val listBy : printer -> printer list -> printer

(** [listAllBy del list] prints elements of [list] delimited by [del]. Empty elements still cause printing delimiters before them *)
val listAllBy : printer -> printer list -> printer

(** [arrayBy del array] prints elements of [array] delimited by [del]. *)
val arrayBy : printer -> printer array -> printer

(** [arrayAllBy del array] prints elements of [array] delimited by [del]. Empty elements still cause printing delimiters before them *)
val arrayAllBy : printer -> printer array -> printer

(** A synonym for [listBy (string "; ")]. *)
val listBySemicolon : printer list -> printer 

(** A synonym for [listBy (string ", ")]. *)
val listByComma : printer list -> printer

(** A synonym for [listBy (string " ")]. *)
val listBySpace : printer list -> printer

(** A synonym for [listBy (seq [string "; "; break])]. *)
val listBySemicolonBreak : printer list -> printer 

(** A synonym for [listBy (seq [string ", "; break])]. *)
val listByCommaBreak : printer list -> printer 

(** A synonym for [listBy (seq [string " "; break])]. *)
val listBySpaceBreak : printer list -> printer 

(** A synonym for [listBy break]. *)
val listByBreak : printer list -> printer 

(** A synonym for [arrayBy (string "; ")]. *)
val arrayBySemicolon : printer array -> printer 

(** A synonym for [arrayBy (string ", ")]. *)
val arrayByComma : printer array -> printer

(** A synonym for [arrayBy (string " ")]. *)
val arrayBySpace : printer array -> printer

(** A synonym for [arrayBy (seq [string "; "; break])]. *)
val arrayBySemicolonBreak : printer array -> printer 

(** A synonym for [arrayBy (seq [string ", "; break])]. *)
val arrayByCommaBreak : printer array -> printer 

(** A synonym for [arrayBy (seq [string " "; break])]. *)
val arrayBySpaceBreak : printer array -> printer 

(** A synonym for [arrayBy break]. *)
val arrayByBreak : printer array -> printer 

(** {2 Shortcuts for boxed formatting} *)

(** Prints its argument within box (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val boxed : printer -> printer

(** Prints its argument within hbox (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val hboxed : printer -> printer

(** Prints its argument within vbox (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val vboxed : printer -> printer

(** Prints its argument within hovbox (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val hovboxed : printer -> printer

(** Prints its argument within hvbox (see standard module 
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} Format}). 
  *)
val hvboxed : printer -> printer

(** [block o c b] prints [b] as a contents of block with opening [o] and
    closing [c].
 *)
val block : printer -> printer -> printer -> printer

(** [plock o b] prints [b] as a contents of prefixed block with 
    opening [o].
 *)
val plock : printer -> printer -> printer 

(** [brboxed left right p] surrounds [p] by "brackets" [left] and [right]. *)
val brboxed : printer -> printer -> printer -> printer

(** Synonym for [brboxed (string "(") (string ")")]. *)
val rboxed : printer -> printer

(** Synonym for [brboxed (string "[") (string "]")]. *)
val sboxed : printer -> printer

(** Synonym for [brboxed (string "{") (string "}")]. *)
val cboxed : printer -> printer
