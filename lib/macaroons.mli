(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

module type CRYPTO = sig
  val hmac : key:string -> string -> string
  val encrypt : key:string -> string -> string
  val decrypt : key:string -> string -> string
end

module type S = sig
  type t

  val create : location:string -> key:string -> id:string -> t

  val location : t -> string

  val identifier : t -> string

  val signature : t -> string

  val add_first_party_caveat : t -> string -> t

  val equal : t -> t -> bool

  val serialize : t -> string

  type unserialize_error =
    [ `Unexpected_char of char
    | `Not_enough_data of int
    | `Unexpected_packet_id of string
    | `Character_not_found of char ]

  val unserialize : string -> [ `Ok of t | `Error of int * unserialize_error ]

  val pp : Format.formatter -> t -> unit

  val verify : t -> key:string -> check:(string -> bool) -> t list -> bool
end

module Make (C : CRYPTO) : S
