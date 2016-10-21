(*
 * Types: common types.
 * Copyright (C) 2016
 * Ekaterina Verbitskaja, St.Petersburg State University
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
 
(** {2 Main parsing types } *)

(** Type pattern for the result of parsing. Here ['a] --- type of {i parsed value}, ['b] --- type of
    {i failure reason} (description of parse problem). Result is 
    {ul {- either a parsed value coupled with optional reason designated to denote deferred errors}
        {- or a failure with optional reason.}
    }
    
    Deferred reasons are those which can be potentially signalled in the future. For example, 
    parsing the string "A, B" with the rule ("A" "B")? has to return parsed value with deferred failure
    reason "B expected".
 *)
type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option

(** The type 
    {C [type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag]}
    denotes the result of parsing a stream with a parser. This result is either parsed value of type 
    ['parsed] and the residual stream of type ['stream], or failure with reason of type ['error].
 *)
type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag

(** The type {C [type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result]}
    corresponds to a parser. Parser takes a stream of type ['stream] and returns result.
 *)
type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result

(** Monadic [bind] for result type *)
val bind : ('stream, 'parsed, 'error) result -> ('parsed -> [`Ok of 'parsed' | `Fail of 'error]) -> ('stream, 'parsed', 'error) result
