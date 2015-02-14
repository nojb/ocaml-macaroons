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

module Result = struct
  type ('a, 'b) t =
    [ `Ok of 'a | `Error of 'b ]
  let (>>=) m f = match m with
    | `Ok a -> f a
    | `Error e -> `Error e
  let return a = `Ok a
  let fail e = `Error e
end

module Make (C : CRYPTO) = struct

  let generate_derived_key k =
    C.hmac ~key:k "macaroons-key-generator"

  type caveat =
    { cid : string;
      vid : string option;
      cl : string option }

  type t =
    { location : string;
      identifier : string;
      signature : string;
      caveats : caveat list }

  let create ~location ~key ~id =
    { location; identifier = id;
      caveats = [];
      signature = generate_derived_key key }

  let location {location} = location

  let identifier {identifier} = identifier

  let signature {signature} = signature

  let add_first_party_caveat m cid =
    let caveats = m.caveats @ [{cid; vid = None; cl = None}] in
    let signature = C.hmac ~key:m.signature cid in
    {m with caveats; signature}

  let equal m1 m2 =
    m1 = m2

  module Writer = struct
    type writer = int * (string -> int -> unit)

    let w_int n =
      let p s o =
        let c i = Char.chr ((n lsr (8*i)) land 0xFF) in
        Bytes.set s (o + 0) (c 0);
        Bytes.set s (o + 1) (c 1);
        Bytes.set s (o + 2) (c 2);
        Bytes.set s (o + 3) (c 3)
      in
      4, p

    let w_string str =
      String.length str, fun s o -> String.blit str 0 s o (String.length str)

    let w_char c =
      1, fun s o -> Bytes.set s o c

    let (<>) (l1, f1) (l2, f2) =
      let l = l1 + l2 in
      let f s o = f1 s o; f2 s (o + l1) in
      l, f

    let w_packet k v =
      let l = String.length k + String.length v + 6 in
      w_int l <> w_string k <> w_char ' ' <> w_string v <> w_char '\n'

    let w_empty =
      0, (fun _ _ -> ())

    let w_option w = function
      | None -> w_empty
      | Some x -> w x

    let w_caveat c =
      w_packet "cid" c.cid <>
      w_option (w_packet "vid") c.vid <>
      w_option (w_packet "cl") c.cl

    let w_macaroon m =
      w_packet "location" m.location <>
      w_packet "identifier" m.identifier <>
      List.fold_left (fun w c -> w <> w_caveat c) w_empty m.caveats <>
      w_packet "signature" m.signature

    let run (l, f) =
      let s = Bytes.create l in
      (f s 0; Bytes.unsafe_to_string s)
  end

  let serialize m =
    B64.encode ~pad:true Writer.(run (w_macaroon m))

  module Reader = struct
    type ('a, 'b) reader = string -> int -> ('a, 'b) Result.t

    type error =
      [ `Unexpected_char of char
      | `Not_enough_data of int
      | `Unexpected_packet_id of string
      | `Character_not_found of char ]

    open Result

    let fail o e = Result.fail (o, e)

    let need l s o =
      if o + l > String.length s then
        fail o (`Not_enough_data l)
      else
        return ()

    let p_int s o =
      need 4 s o >>= fun () ->
      let c i = Char.code (s.[o + i]) lsl (8*i) in
      let n = c 0 lor c 1 lor c 2 lor c 3 in
      return (n, o + 4)

    let p_string l s o =
      need l s o >>= fun () ->
      return (String.sub s o l, o + l)

    let p_char c s o =
      need 1 s o >>= fun () ->
      if s.[o] <> c then fail o (`Unexpected_char c)
      else return (o + 1)

    let index c s o =
      try
        return (String.index_from s o c)
      with
      | Not_found -> fail o (`Character_not_found c)

    let p_packet s o =
      p_int s o >>= fun (l, o) ->
      let oend = o + l in
      index ' ' s (o + 4) >>= fun i ->
      let l = i - o - 4 in
      p_string l s (o + 4) >>= fun (k, o) ->
      p_char ' ' s o >>= fun o ->
      p_string (oend - o - 1) s o >>= fun (v, o) ->
      p_char '\n' s o >>= fun o ->
      return ((k, v), o)

    let p_named_packet n s o =
      p_packet s o >>= function
      | ((k, v), o) when k = n -> return (v, o)
      | ((k, _), _) -> fail o (`Unexpected_packet_id k)

    let p_macaroon s o =
      p_named_packet "location" s o >>= fun (location, o) ->
      p_named_packet "identifier" s o >>= fun (identifier, o) ->
      let rec loop caveats c = function
        | (("cid", cid), o) ->
          let caveats = {c with cid = cid} :: caveats in
          p_packet s o >>= loop caveats {cid = ""; vid = None; cl = None}
        | (("vid", vid), o) ->
          p_packet s o >>= loop caveats {c with vid = Some vid}
        | (("cl", cl), o) ->
          p_packet s o >>= loop caveats {c with cl = Some cl}
        | (("signature", signature), o) ->
          let caveats = List.rev caveats in
          return { location; identifier; caveats; signature }
        | ((k, _), _) ->
          fail o (`Unexpected_packet_id k)
      in
      p_packet s o >>= loop [] {cid = ""; vid = None; cl = None}
  end

  type unserialize_error = Reader.error

  let unserialize s =
    Reader.p_macaroon (B64.decode s) 0

  let pp ppf m =
    let hex s = let `Hex s = Hex.of_string ~pretty:true s in s in
    Format.fprintf ppf "location@ %s\n" m.location;
    Format.fprintf ppf "identifier@ %s\n" m.identifier;
    List.iter (function c -> Format.fprintf ppf "cid@ %s\n" c.cid) m.caveats;
    Format.fprintf ppf "signature@ %s\n" (hex m.signature)

  open Result

  let verify m ~key ~check d =
    (* let rec verify2 rs rk m = *)
    (*   let rec loop cs = function *)
    (*     | { cid; vid = None } :: caveats -> *)
    (*       if check cid then *)
    (*         loop (C.hmac ~key:cs cid) caveats *)
    (*       else *)
    (*         fail () *)
    (*     | { cid; vid = Some vid } :: caveats -> *)
    (*       catch (List.find (fun m -> m.identifier = cid) d) (fun _ -> fail ()) >>= fun m -> *)
    (*       verify2 rs (C.decrypt ~key:cs vid) m >>= fun _ -> *)
    (*       loop (C.hmac ~key:cs (vid ^ cid)) caveats *)
    (*     | [] -> *)
    (*       return cs *)
    (*   in *)
    (*   match loop (C.hmac ~key:rk m.identifier) m.caveats with *)
    (*   | `Error _ -> false *)
    (*   | `Ok cs -> *)
    (*     bind_for_request rs cs = m.signature *)
    (* in *)
    (* verify2 m.signature (generate_derived_key key) m *)
    assert false

end
