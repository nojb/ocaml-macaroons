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

module SodiumCrypto : Macaroons.CRYPTO = struct
  open Sodium

  module A = Auth.Bytes

  let fix_length s len =
    let l = String.length s in
    if len < l then String.sub s 0 len
    else if len > l then s ^ (Bytes.make (len - l) '\000')
    else s

  let hmac ~key m =
    let key = fix_length key Auth.key_size in
    A.of_auth (A.auth (A.to_key key) m)

  module H = Hash.Bytes

  let hash m =
    H.of_hash (H.digest m)

  module B = Secret_box.Bytes

  (* FIXME use random nonce *)
  let nonce =
    Secret_box.nonce_of_bytes (Bytes.make Secret_box.nonce_size '\000')

  let encrypt ~key m =
    B.secret_box (B.to_key key) m nonce

  let decrypt ~key m =
    B.secret_box_open (B.to_key key) m nonce
end

include Macaroons.Make (SodiumCrypto)
