open Core.Std

(** A type for bencoded values *)
type t = S of string
       | I of int
       | L of t list
       | D of (string * t) list

(** Decodes a bencoded string into a t and a leftover string *)
val decode : string -> (t * string) option
