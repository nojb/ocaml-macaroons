open OUnit2

module M = Sodium_macaroons

let secret = "this is our super secret key; only we should know it"
let public = "we used our secret key"
let location = "http://mybank/"

let m0 = M.create ~location ~key:secret ~id:public

let test_serialize m ctxt =
  match M.unserialize (M.serialize m) with
  | `Ok m' -> assert_equal ~ctxt m m'
  | `Error _ -> assert false

let m1 = M.add_first_party_caveat m0 "account = 3735928559"
let m1 = M.add_first_party_caveat m0 "time < 2015-01-01T00:00"
let m1 = M.add_first_party_caveat m0 "email = alice@example.org"

let check_zero _ = false
let check_all _ = true

let test_verify m ~key ~check d b ctxt =
  assert_equal ~ctxt (M.verify m ~key ~check d) b

let secret' = "this is a different super-secret key; never use the same secret twice"
let public' = "we used our other secret key"
let location' = "http://mybank/"
let m2 = M.create ~location:location' ~key:secret' ~id:public'
let m2 = M.add_first_party_caveat m2 "account = 3735928559"
let caveat_key = "4; guaranteed random by a fair toss of the dice"
let predicate = "user = Alice"
let identifier = "this was how we remind auth of key/pred"
let m3 = M.add_third_party_caveat m2 ~key:caveat_key identifier
let d = M.create ~location:"http://auth.mybank/" ~key:caveat_key ~id:identifier
let d = M.add_first_party_caveat d "time < 2015-01-01T00:00"
let dp = M.prepare_for_request m3 d

let suite =
  "suite" >:::
  [ "test_serialize_basic" >:: test_serialize m0;
    "test_serialize_first_party" >:: test_serialize m1;
    "test_verify_fail_key" >:: test_verify m1 ~key:"bad" ~check:check_zero [] false;
    "test_verify_fail_first_party" >:: test_verify m1 ~key:secret ~check:check_zero [] false;
    "test_verify_success_first_party" >:: test_verify m1 ~key:secret ~check:check_all [] true;
    "test_serialize_third_party" >:: test_serialize m3;
    "test_verify_fail_prepare" >:: test_verify m3 ~key:secret' ~check:check_all [d] false;
    "test_verify_success_prepare" >:: test_verify m3 ~key:secret' ~check:check_all [dp] true;
    "test_verify_third_party" >:: test_verify m3 ~key:secret' ~check:check_all [] false;
    "test_verify_third_party2" >:: test_verify m3 ~key:secret' ~check:check_zero [dp] false ]

let _ =
  run_test_tt_main suite
