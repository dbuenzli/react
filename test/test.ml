(*---------------------------------------------------------------------------
   Copyright (c) 2009 The react programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Tests for react's combinators.
   Compile with -g to get a precise backtrace to the error.

   Note that the testing mechanism itself (cf. occs and vals) needs a correct
   implementation; particulary w.r.t. updates with side effects. *)

open React;;

let pp_list ppv pp l =
  Format.fprintf pp "@[[";
  List.iter (fun v -> Format.fprintf pp "%a;@ " ppv v) l;
  Format.fprintf pp "]@]"

let pr_value pp name v = Format.printf "@[<hov 2>%s =@ %a@]@." name pp v
let e_pr ?iff pp name e = E.trace ?iff (pr_value pp name) e
let s_pr ?iff pp name s = S.trace ?iff (pr_value pp name) s

(* Tests the event e has occurences occs. *)
let occs ?(eq = ( = )) e occs =
  let occs = ref occs in
  let assert_occ o = match !occs with
  | o' :: occs' when eq o' o -> occs := occs'
  | _ -> assert false
  in
  E.map assert_occ e, occs

(* Tests the signal s goes through vals. *)
let vals ?(eq = ( = )) s vals =
  let vals = ref vals in
  let assert_val v = match !vals with
  | v' :: vals' when eq v' v -> vals := vals'
  | _ -> assert false
  in
  S.map assert_val s, vals

(* Tests that we went through all vals or occs *)
let empty (_, r) = assert (!r = [])

(* To initialize asserts of dynamic creations. *)
let assert_e_stub () = ref (occs E.never [])
let assert_s_stub v = ref (vals (S.const v) [v])

(* To keep references for the g.c. (warning also stops the given nodes) *)
let keep_eref e = E.stop e
let keep_sref s = S.stop s

(* To artificially raise the rank of events and signals *)
let high_e e =
  let id e = E.map (fun v -> v) e in (id (id (id (id (id (id (id (id e))))))))

let high_s s =
  let id s = S.map (fun v -> v) s in (id (id (id (id (id (id (id (id s))))))))

(* Event tests *)

let test_no_leak () =
  let x, send_x = E.create () in
  let count = ref 0 in
  let w =
    let w = Weak.create 1 in
    let e = E.map (fun x -> incr count) x in
    Weak.set w 0 (Some e);
    w
  in
  List.iter send_x [0; 1; 2];
  Gc.full_major ();
  List.iter send_x [3; 4; 5];
  (match Weak.get w 0 with None -> () | Some _ -> assert false);
  if !count > 3 then assert false else ()

let test_once_drop_once () =
  let w, send_w = E.create () in
  let x = E.once w in
  let y = E.drop_once w in
  let assert_x = occs x [0] in
  let assert_y = occs y [1; 2; 3] in
  let assert_dx = assert_e_stub () in
  let assert_dy = assert_e_stub () in
  let dyn () =
    let dx = E.once w in
    let dy = E.drop_once w in
    assert_dx := occs dx [1];
    assert_dy := occs dy [2; 3]
  in
  let create_dyn = E.map (fun v -> if v = 1 then dyn ()) w in
  Gc.full_major ();
  List.iter send_w [0; 1; 2; 3];
  List.iter empty [assert_x; assert_y; !assert_dx; !assert_dy];
  keep_eref create_dyn

let test_app () =
  let f x y = x + y in
  let w, send_w = E.create () in
  let x = E.map (fun w -> f w) w in
  let y = E.drop_once w in
  let z = E.app x y in
  let assert_z = occs z [ 2; 4; 6 ] in
  let assert_dz = assert_e_stub () in
  let dyn () =
    let dx = E.drop_once (E.map (fun w -> f w) w) in
    let dz = E.app dx y in
    assert_dz := occs dz [ 4; 6 ];
  in
  let create_dyn = E.map (fun v -> if v = 1 then dyn ()) w in
  Gc.full_major ();
  List.iter send_w [0; 1; 2; 3];
  List.iter empty [assert_z; !assert_dz];
  keep_eref create_dyn

let test_map_stamp_filter_fmap () =
  let v, send_v = E.create () in
  let w = E.map (fun s -> "z:" ^ s) v in
  let x = E.stamp v "bla" in
  let y = E.filter (fun s -> String.length s = 5) v in
  let z = E.fmap (fun s -> if s = "blu" then Some "hip" else None) v in
  let assert_w = occs w ["z:didap"; "z:dip"; "z:didop"; "z:blu"] in
  let assert_x = occs x ["bla"; "bla"; "bla"; "bla"] in
  let assert_y = occs y ["didap"; "didop"] in
  let assert_z = occs z ["hip"] in
  let assert_dw = assert_e_stub () in
  let assert_dx = assert_e_stub () in
  let assert_dy = assert_e_stub () in
  let assert_dz = assert_e_stub () in
  let dyn () =
    let dw = E.map (fun s -> String.length s) v  in
    let dx = E.stamp v 4 in
    let dy = E.filter (fun s -> String.length s = 5) v in
    let dz = E.fmap (fun s -> if s = "didap" then Some "ha" else None) v in
    let _ = E.map (fun _ -> assert false) (E.fmap (fun _ -> None) x) in
    assert_dw := occs dw [5; 3; 5; 3];
    assert_dx := occs dx [4; 4; 4; 4];
    assert_dy := occs dy ["didap"; "didop"];
    assert_dz := occs dz ["ha"];
  in
  let create_dyn = E.map (fun v -> if v = "didap" then dyn ()) v in
  Gc.full_major ();
  List.iter send_v ["didap"; "dip"; "didop"; "blu"];
  List.iter empty [assert_w; assert_x; assert_y; assert_z];
  List.iter empty [!assert_dw; !assert_dx];
  List.iter empty [!assert_dy; !assert_dz];
  keep_eref create_dyn

let test_diff_changes () =
  let x, send_x = E.create () in
  let y = E.diff ( - ) x in
  let z = E.changes x in
  let assert_y = occs y [ 0; 1; 1; 0] in
  let assert_z = occs z [ 1; 2; 3] in
  let assert_dy = assert_e_stub () in
  let assert_dz = assert_e_stub () in
  let dyn () =
    let dy = E.diff ( - ) x in
    let dz = E.changes z in
    assert_dy := occs dy [1; 0];
    assert_dz := occs dz [2; 3];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 3; 3];
  List.iter empty [assert_y; assert_z; !assert_dy; !assert_dz];
  keep_eref create_dyn

let test_dismiss () =
  let x, send_x = E.create () in
  let y = E.fmap (fun x -> if x mod 2 = 0 then Some x else None) x in
  let z = E.dismiss y x in
  let assert_z = occs z [1; 3; 5] in
  let assert_dz = assert_e_stub () in
  let dyn () =
    let dz = E.dismiss y x in
    assert_dz := occs dz [3; 5];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn()) x in
  Gc.full_major ();
  List.iter send_x [0; 1; 2; 3; 4; 5];
  List.iter empty [assert_z; !assert_dz];
  keep_eref create_dyn

let test_on () =
  let e, send_e = E.create () in
  let s = S.hold 0 e in
  let c = S.map (fun x -> x mod 2 = 0) s in
  let w = E.on c e in
  let ovals = [2; 4; 4; 6; 4] in
  let assert_w = occs w ovals in
  let assert_dw = assert_e_stub () in
  let assert_dhw = assert_e_stub () in
  let dyn () =
    let dw = E.on c e in
    let dhw = E.on (high_s c) (high_e e) in
    assert_dw := occs dw ovals;
    assert_dhw := occs dhw ovals
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) e in
  Gc.full_major ();
  List.iter send_e [ 1; 3; 1; 2; 4; 4; 6; 1; 3; 4 ];
  List.iter empty [assert_w; !assert_dw; !assert_dhw ];
  keep_eref create_dyn

let test_until () =
  let x, send_x = E.create () in
  let stop = E.filter (fun v -> v = 3) x in
  let e = E.until stop x in
  let assert_e = occs e [1; 2] in
  let assert_de = assert_e_stub () in
  let assert_de' = assert_e_stub () in
  let dyn () =
    let de = E.until stop x in
    let de' = E.until (E.filter (fun v -> v = 5) x) x in
    assert_de := occs de [];
    assert_de' := occs de' [3; 4]
  in
  let create_dyn = E.map (fun v -> if v = 3 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 2; 3; 4; 5];
  List.iter empty [assert_e; !assert_de; !assert_de'];
  keep_eref create_dyn

let test_accum () =
  let f, send_f = E.create () in
  let a = E.accum f 0 in
  let assert_a = occs a [2; -1; -2] in
  let assert_da = assert_e_stub () in
  let dyn () =
    let da = E.accum f 0 in
    assert_da := occs da [1; 2];
  in
  let create_dyn =
    let count = ref 0 in
    E.map (fun _ -> incr count; if !count = 2 then dyn ()) f
  in
  Gc.full_major ();
  List.iter send_f [( + ) 2; ( - ) 1; ( * ) 2];
  List.iter empty [assert_a; !assert_da];
  keep_eref create_dyn

let test_fold () =
  let x, send_x = E.create () in
  let c = E.fold ( + ) 0 x in
  let assert_c = occs c [1; 3; 6; 10] in
  let assert_dc = assert_e_stub () in
  let dyn () =
    let dc = E.fold ( + ) 0 x in
    assert_dc := occs dc [2; 5; 9];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 2; 3; 4];
  List.iter empty [assert_c; !assert_dc];
  keep_eref create_dyn

let test_select () =
  let w, send_w = E.create () in
  let x, send_x = E.create () in
  let y = E.map succ w in
  let z = E.map succ y in
  let tw = E.map (fun v -> `Int v) w in
  let tx = E.map (fun v -> `Bool v) x in
  let t = E.select [tw; tx] in
  let sy = E.select [y; z] in (* always y. *)
  let sz = E.select [z; y] in (* always z. *)
  let assert_t = occs t [ `Int 0; `Bool false; `Int 1; `Int 2; `Int 3 ] in
  let assert_sy = occs sy [1; 2; 3; 4] in
  let assert_sz = occs sz [2; 3; 4; 5] in
  let assert_d = assert_e_stub () in
  let dyn () =
    let d = E.select [y; w; z] in
    assert_d := occs d [3; 4]
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) w in
  Gc.full_major ();
  send_w 0; send_x false; List.iter send_w [1; 2; 3;];
  empty assert_t; List.iter empty [assert_sy; assert_sz; !assert_d];
  keep_eref create_dyn

let test_merge () =
  let w, send_w = E.create () in
  let x, send_x = E.create () in
  let y = E.map succ w in
  let z = E.merge (fun acc v -> v :: acc) [] [w; x; y] in
  let assert_z = occs z [[2; 1]; [4]; [3; 2]] in
  let assert_dz = assert_e_stub () in
  let dyn () =
    let dz = E.merge (fun acc v -> v :: acc) [] [y; x; w] in
    assert_dz := occs dz [[4]; [2; 3]]
  in
  let create_dyn = E.map (fun v -> if v = 4 then dyn ()) x in
  Gc.full_major ();
  send_w 1; send_x 4; send_w 2;
  List.iter empty [assert_z; !assert_dz];
  keep_eref create_dyn

let test_switch () =
  let x, send_x = E.create () in
  let switch e =
    E.fmap (fun v -> if v mod 3 = 0 then Some (E.map (( * ) v) e) else None) x
  in
  let s = E.switch x (switch x) in
  let hs = E.switch x (switch (high_e x)) in
  let assert_s = occs s [1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_hs = occs hs [1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_ds = assert_e_stub () in
  let assert_dhs = assert_e_stub () in
  let dyn () =
    let ds = E.switch x (switch x) in
    let dhs = E.switch x (switch (high_e x)) in
    assert_ds := occs ds [9; 12; 15; 36; 42; 48; 81];
    assert_ds := occs dhs [9; 12; 15; 36; 42; 48; 81]
  in
  let create_dyn = E.map (fun v -> if v = 3 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 2; 3; 4; 5; 6; 7; 8; 9];
  List.iter empty [assert_s; assert_hs; !assert_ds; !assert_dhs];
  keep_eref create_dyn

let test_fix () =
  let x, send_x = E.create () in
  let c1 () = E.stamp x `C2 in
  let c2 () = E.stamp x `C1 in
  let loop result =
    let switch = function `C1 -> c1 () | `C2 -> c2 () in
    let switcher = E.switch (c1 ()) (E.map switch result) in
    switcher, switcher
  in
  let l = E.fix loop in
  let assert_l = occs l [`C2; `C1; `C2] in
  let assert_dl = assert_e_stub () in
  let dyn () =
    let dl = E.fix loop in
    assert_dl := occs dl [`C2; `C1];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 2; 3];
  List.iter empty [assert_l; !assert_dl];
  keep_eref create_dyn

let test_lifts () =
  let x1, send_x1 = E.create () in
  let x2, send_x2 = E.create () in
  let x3, send_x3 = E.create () in
  let x4, send_x4 = E.create () in
  let x5, send_x5 = E.create () in
  let x6, send_x6 = E.create () in
  let f1 a = 1 + a in
  let f2 a0 a1 = a0 + a1 in
  let f3 a0 a1 a2 = a0 + a1 + a2 in
  let f4 a0 a1 a2 a3 = a0 + a1 + a2 + a3 in
  let f5 a0 a1 a2 a3 a4 = a0 + a1 + a2 + a3 + a4 in
  let f6 a0 a1 a2 a3 a4 a5 = a0 + a1 + a2 + a3 + a4 + a5 in
  let v1 = E.l1 f1 x1 in
  let v2 = E.l2 f2 x1 x2 in
  let v3 = E.l3 f3 x1 x2 x3 in
  let v4 = E.l4 f4 x1 x2 x3 x4 in
  let v5 = E.l5 f5 x1 x2 x3 x4 x5 in
  let v6 = E.l6 f6 x1 x2 x3 x4 x5 x6 in
  let a_v1 = occs v1 [2; 2; 2; 2; 2; 2;] in
  let a_v2 = occs v2 [   3; 3; 3; 3; 3;] in
  let a_v3 = occs v3 [      6; 6; 6; 6;] in
  let a_v4 = occs v4 [        10;10;10;] in
  let a_v5 = occs v5 [           15;15;] in
  let a_v6 = occs v6 [              21;] in
  let with_step f =
    let s = Step.create () in
    f s; Step.execute s
  in
  let s1 s = send_x1 ~step:s 1 in
  let s2 s = s1 s; send_x2 ~step:s 2 in
  let s3 s = s2 s; send_x3 ~step:s 3 in
  let s4 s = s3 s; send_x4 ~step:s 4 in
  let s5 s = s4 s; send_x5 ~step:s 5 in
  let s6 s = s5 s; send_x6 ~step:s 6 in
  with_step s1; with_step s2; with_step s3;
  with_step s4; with_step s5; with_step s6;
  List.iter empty [ a_v1; a_v2; a_v3; a_v4; a_v5; a_v6;];
  ()

let test_option () =
  let x, send_x = E.create () in
  let s, set_s = S.create 4 in
  let some = E.Option.some (S.changes s) in
  let e0 = E.Option.value x in
  let e1 = E.Option.value ~default:(S.const 2) x in
  let e2 = E.Option.value ~default:s x in
  let assert_some = occs some [ Some 42;] in
  let assert_e0 = occs e0 [1; 5; ] in
  let assert_e1 = occs e1 [1; 2; 5; 2] in
  let assert_e2 = occs e2 [1; 4; 5; 42] in
  send_x (Some 1); send_x None; set_s 42;
  send_x (Some 5); send_x None;
  empty assert_some;
  List.iter empty [ assert_e0; assert_e1; assert_e2];
  ()

let test_events () =
  test_no_leak ();
  test_once_drop_once ();
  test_app ();
  test_map_stamp_filter_fmap ();
  test_diff_changes ();
  test_on ();
  test_dismiss ();
  test_until ();
  test_accum ();
  test_fold ();
  test_select ();
  test_merge ();
  test_switch ();
  test_fix ();
  test_lifts ();
  test_option ();
  ()

(* Signal tests *)

let test_no_leak () =
  let x, set_x = S.create 0 in
  let count = ref 0 in
  let w =
    let w = Weak.create 1 in
    let e = S.map (fun x -> incr count) x in
    Weak.set w 0 (Some e);
    w
  in
  List.iter set_x [ 0; 1; 2];
  Gc.full_major ();
  List.iter set_x [ 3; 4; 5];
  (match Weak.get w 0 with None -> () | Some _ -> assert false);
  if !count > 3 then assert false else ()

let test_hold () =
  let e, send_e = E.create () in
  let e', send_e' = E.create () in
  let he = high_e e in
  let s = S.hold 1 e in
  let assert_s = vals s  [1; 2; 3; 4] in
  let assert_ds = assert_s_stub 0 in
  let assert_dhs = assert_s_stub 0 in
  let assert_ds' = assert_s_stub 0 in
  let dyn () =
    let ds = S.hold 42 e in                            (* init value unused. *)
    let dhs = S.hold 44 he in                          (* init value unused. *)
    let ds' = S.hold 128 e' in                           (* init value used. *)
    assert_ds := vals ds [3; 4];
    assert_dhs := vals dhs [3; 4];
    assert_ds' := vals ds' [128; 2; 4]
  in
  let create_dyn = S.map (fun v -> if v = 3 then dyn ()) s in
  Gc.full_major ();
  List.iter send_e [ 1; 1; 1; 1; 2; 2; 2; 3; 3; 3];
  List.iter send_e' [2; 4];
  List.iter send_e [4; 4; 4];
  List.iter empty [assert_s; !assert_ds; !assert_dhs; !assert_ds'];
  keep_sref create_dyn

let test_app () =
  let f x y = x + y in
  let fl x y = S.app (S.app ~eq:(==) (S.const f) x) y in
  let x, set_x = S.create 0 in
  let y, set_y = S.create 0 in
  let z = fl x y in
  let assert_z = vals z [ 0; 1; 3; 4 ] in
  let assert_dz = assert_s_stub 0 in
  let assert_dhz = assert_s_stub 0 in
  let dyn () =
    let dz = fl x y in
    let dhz = fl (high_s x) (high_s y) in
    assert_dz := vals dz [3; 4];
    assert_dhz := vals dhz [3; 4];
  in
  let create_dyn = S.map (fun v -> if v = 2 then dyn ()) y in
  Gc.full_major ();
  set_x 1; set_y 2; set_x 1; set_y 3;
  List.iter empty [assert_z; !assert_dz; !assert_dhz];
  keep_sref create_dyn

let test_map_filter_fmap () =
  let even x = x mod 2 = 0 in
  let odd x = x mod 2 <> 0 in
  let meven x = if even x then Some (x * 2) else None in
  let modd x = if odd x then Some (x * 2) else None in
  let double x = 2 * x in
  let x, set_x = S.create 1 in
  let x2 = S.map double x in
  let fe = S.filter even 56 x in
  let fo = S.filter odd 56 x in
  let fme = S.fmap meven 7 x in
  let fmo = S.fmap modd 7 x in
  let assert_x2 = vals x2 [ 2; 4; 6; 8; 10] in
  let assert_fe = vals fe [ 56; 2; 4;] in
  let assert_fo = vals fo [ 1; 3; 5] in
  let assert_fme = vals fme [ 7; 4; 8;] in
  let assert_fmo = vals fmo [ 2; 6; 10;] in
  let assert_dx2 = assert_s_stub 0 in
  let assert_dhx2 = assert_s_stub 0 in
  let assert_dfe = assert_s_stub 0 in
  let assert_dhfe = assert_s_stub 0 in
  let assert_dfo = assert_s_stub 0 in
  let assert_dhfo = assert_s_stub 0 in
  let assert_dfme = assert_s_stub 0 in
  let assert_dhfme = assert_s_stub 0 in
  let assert_dfmo = assert_s_stub 0 in
  let assert_dhfmo = assert_s_stub 0 in
  let dyn () =
    let dx2 = S.map double x in
    let dhx2 = S.map double (high_s x) in
    let dfe = S.filter even 56 x in
    let dhfe = S.filter even 56 (high_s x) in
    let dfo = S.filter odd 56 x in
    let dhfo = S.filter odd 56 (high_s x) in
    let dfme = S.fmap meven 7 x in
    let dhfme = S.fmap meven 7 (high_s x) in
    let dfmo = S.fmap modd 7 x in
    let dhfmo = S.fmap modd 7 (high_s x) in
    assert_dx2 := vals dx2 [6; 8; 10];
    assert_dhx2 := vals dhx2 [6; 8; 10];
    assert_dfe := vals dfe [56; 4];
    assert_dhfe := vals dhfe [56; 4];
    assert_dfo := vals dfo [3; 5];
    assert_dhfo := vals dhfo [3; 5];
    assert_dfme := vals dfme [7; 8;];
    assert_dhfme := vals dhfme [7; 8;];
    assert_dfmo := vals dfmo [6; 10];
    assert_dhfmo := vals dhfmo [6; 10];
    ()
  in
  let create_dyn = S.map (fun v -> if v = 3 then dyn ()) x in
  Gc.full_major ();
  List.iter set_x [ 1; 2; 3; 4; 4; 5; 5];
  List.iter empty [assert_x2; assert_fe; assert_fo; assert_fme;
                   assert_fmo; !assert_dx2; !assert_dhx2; !assert_dfe;
                   !assert_dhfe; !assert_dfo ; !assert_dhfo; !assert_dfme ;
                   !assert_dhfme ; !assert_dfmo ; !assert_dhfmo ];
  keep_sref create_dyn


let test_diff_changes () =
  let e, send_e = E.create () in
  let s = S.hold 1 e in
  let d = S.diff (fun x y -> x - y) s in
  let c = S.changes s in
  let assert_dd = assert_e_stub () in
  let assert_dhd = assert_e_stub () in
  let assert_dc = assert_e_stub () in
  let assert_dhc = assert_e_stub () in
  let dyn () =
    let dd = S.diff (fun x y -> x - y) s in
    let dhd = S.diff (fun x y -> x - y) (high_s s) in
    let dc = S.changes s in
    let dhc = S.changes (high_s s) in
    assert_dd := occs dd [1];
    assert_dhd := occs dhd [1];
    assert_dc := occs dc [4];
    assert_dhc := occs dhc [4]
  in
  let create_dyn = S.map (fun v -> if v = 3 then dyn ()) s in
  let assert_d = occs d [2; 1] in
  let assert_c = occs c [3; 4] in
  Gc.full_major ();
  List.iter send_e [1; 1; 3; 3; 4; 4];
  List.iter empty [assert_d; assert_c; !assert_dd; !assert_dhd; !assert_dc;
                   !assert_dhc];
  keep_sref create_dyn

let test_sample () =
  let pair v v' = v, v' in
  let e, send_e = E.create () in
  let sampler () = E.filter (fun x -> x mod 2 = 0) e in
  let s = S.hold 0 e in
  let sam = S.sample pair (sampler ()) s in
  let ovals = [ (2, 2); (2, 2); (4, 4); (4, 4)] in
  let assert_sam = occs sam ovals in
  let assert_dsam = assert_e_stub () in
  let assert_dhsam = assert_e_stub () in
  let dyn () =
    let dsam = S.sample pair (sampler ()) s in
    let dhsam = S.sample pair (high_e (sampler ())) (high_s s) in
    assert_dsam := occs dsam ovals;
    assert_dhsam := occs dhsam ovals
  in
  let create_dyn = S.map (fun v -> if v = 2 then dyn ()) s in
  Gc.full_major ();
  List.iter send_e [1; 1; 2; 2; 3; 3; 4; 4];
  List.iter empty [assert_sam; !assert_dsam; !assert_dhsam];
  keep_sref create_dyn

let test_on () =
  let s, set_s = S.create 0 in
  let ce = S.map (fun x -> x mod 2 = 0) s in
  let co = S.map (fun x -> x mod 2 <> 0) s in
  let se = S.on ce 42 s in
  let so = S.on co 56 s in
  let assert_se = vals se [ 0; 2; 4; 6; 4 ] in
  let assert_so = vals so [ 56; 1; 3; 1; 3 ] in
  let assert_dse = assert_s_stub 0 in
  let assert_dhse = assert_s_stub 0 in
  let assert_dso = assert_s_stub 0 in
  let assert_dhso = assert_s_stub 0 in
  let dyn () =
    let dse = S.on ce 42 s in
    let dhse = S.on ce 42 (high_s s) in
    let dso = S.on co 56 s in
    let dhso = S.on co 56 (high_s s) in
    assert_dse := vals dse [6; 4];
    assert_dhse := vals dhse [6; 4];
    assert_dso := vals dso [56; 1; 3];
    assert_dhso := vals dhso [56; 1; 3 ]
  in
  let create_dyn = S.map (fun v -> if v = 6 then dyn ()) s in
  Gc.full_major ();
  List.iter set_s [ 1; 3; 1; 2; 4; 4; 6; 1; 3; 4 ];
  List.iter empty [assert_se; assert_so; !assert_dse; !assert_dhse;
                   !assert_dso; !assert_dhso];
  keep_sref create_dyn

let test_dismiss () =
  let x, send_x = E.create () in
  let y = E.fmap (fun x -> if x mod 2 = 0 then Some x else None) x in
  let z = S.dismiss y 4 (S.hold 44 x) in
  let assert_z = vals z [44; 1; 3; 5] in
  let assert_dz = assert_s_stub 0 in
  let dyn () =
    let dz = S.dismiss y 4 (S.hold 44 x) in
    assert_dz := vals dz [4; 3; 5];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn()) x in
  Gc.full_major ();
  List.iter send_x [0; 1; 2; 3; 4; 5];
  List.iter empty [assert_z; !assert_dz];
  keep_eref create_dyn

let test_accum () =
  let f, send_f = E.create () in
  let a = S.accum f 0 in
  let assert_a = vals a [ 0; 2; -1; -2] in
  let assert_da = assert_s_stub 0 in
  let assert_dha = assert_s_stub 0 in
  let dyn () =
    let da = S.accum f 3 in
    let dha = S.accum (high_e f) 3 in
    assert_da := vals da [-2; -4];
    assert_dha := vals dha [-2; -4]
  in
  let create_dyn =
    let count = ref 0 in
    E.map (fun _  -> incr count; if !count = 2 then dyn()) f
  in
  Gc.full_major ();
  List.iter send_f [( + ) 2; ( - ) 1; ( * ) 2];
  List.iter empty [assert_a; !assert_da; !assert_dha];
  keep_eref create_dyn

let test_fold () =
  let x, send_x = E.create () in
  let c = S.fold ( + ) 0 x in
  let assert_c = vals c [ 0; 1; 3; 6; 10] in
  let assert_dc = assert_s_stub 0 in
  let assert_dhc = assert_s_stub 0 in
  let dyn () =
    let dc = S.fold ( + ) 2 x in
    let dhc = S.fold ( + ) 2 (high_e x) in
    assert_dc := vals dc [4; 7; 11];
    assert_dhc := vals dhc [4; 7; 11]
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 2; 3; 4];
  List.iter empty [assert_c; !assert_dc; !assert_dhc ];
  keep_eref create_dyn

let test_merge () =
  let cons acc v = v :: acc in
  let w, set_w = S.create 0 in
  let x, set_x = S.create 1 in
  let y = S.map succ w in
  let z = S.map List.rev (S.merge cons [] [w; x; y]) in
  let assert_z = vals z [[0; 1; 1]; [1; 1; 2]; [1; 4; 2]; [2; 4; 3]] in
  let assert_dz = assert_s_stub [] in
  let assert_dhz = assert_s_stub [] in
  let dyn () =
    let dz = S.map List.rev (S.merge cons [] [w; x; y]) in
    let dhz = S.map List.rev (S.merge cons [] [(high_s w); x; y; S.const 2]) in
    assert_dz := vals dz [[1; 4; 2]; [2; 4; 3]];
    assert_dhz := vals dhz [[1; 4; 2; 2]; [2; 4; 3; 2]]
  in
  let create_dyn = S.map (fun v -> if v = 4 then dyn ()) x in
  Gc.full_major ();
  set_w 1; set_x 4; set_w 2; set_w 2;
  List.iter empty [assert_z; !assert_dz; !assert_dhz];
  keep_sref create_dyn

let esswitch s es = (* Pre 1.0.0 S.switch *)
  S.switch (S.hold ~eq:( == ) s es)

let test_switch () =
  let s, set_s = S.create 0 in
  let switch s =
    let map v =
      if v mod 3 = 0 && v <> 0 then Some (S.map (( * ) v) s) else None
    in
    S.fmap ~eq:( == ) map s s
  in
  let sw = S.switch (switch s) in
  let hsw = S.switch (switch (high_s s)) in
  let assert_sw = vals sw [0; 1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_hsw = vals hsw [0; 1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_dsw = assert_s_stub 0 in
  let assert_dhsw = assert_s_stub 0 in
  let dyn () =
    let dsw = S.switch (switch s) in
    let dhsw = S.switch (switch (high_s s)) in
    assert_dsw := vals dsw [9; 12; 15; 36; 42; 48; 81];
    assert_dhsw := vals dhsw [9; 12; 15; 36; 42; 48; 81];
  in
  let create_dyn = S.map (fun v -> if v = 3 then dyn ()) s in
  Gc.full_major ();
  List.iter set_s [1; 1; 2; 2; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9];
  List.iter empty [assert_sw; assert_hsw; !assert_dsw; !assert_dhsw ];
  keep_sref create_dyn

let test_esswitch () =
  let x, send_x = E.create () in
  let s = S.hold 0 x in
  let switch s =
    E.fmap (fun v -> if v mod 3 = 0 then Some (S.map (( * ) v) s) else None) x
  in
  let sw = esswitch s (switch s) in
  let hsw = esswitch s (switch (high_s s)) in
  let assert_sw = vals sw [0; 1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_hsw = vals hsw [0; 1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_dsw = assert_s_stub 0 in
  let assert_dhsw = assert_s_stub 0 in
  let dyn () =
    let dsw = esswitch s (switch s) in
    let dhsw = esswitch s (switch (high_s s)) in
    assert_dsw := vals dsw [9; 12; 15; 36; 42; 48; 81];
    assert_dhsw := vals dhsw [9; 12; 15; 36; 42; 48; 81];
  in
  let create_dyn = E.map (fun v -> if v = 3 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 2; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9];
  List.iter empty [assert_sw; assert_hsw; !assert_dsw; !assert_dhsw ];
  keep_eref create_dyn

let test_switch_const () =
  let s, set_s = S.create 0 in
  let switch = S.map (fun x -> S.const x) s in
  let sw = S.switch switch in
  let assert_sw = vals sw [0; 1; 2; 3] in
  let assert_dsw = assert_s_stub 0 in
  let dyn () =
    let dsw = S.switch switch in
    assert_dsw := vals dsw [2; 3];
  in
  let create_dyn = S.map (fun v -> if v = 2 then dyn ()) s in
  Gc.full_major ();
  List.iter set_s [0; 1; 2; 3];
  List.iter empty [assert_sw; !assert_dsw ];
  keep_sref create_dyn

let test_esswitch_const () =
  let x, send_x = E.create () in
  let switch = E.map (fun x -> S.const x) x in
  let sw = esswitch (S.const 0) switch in
  let assert_sw = vals sw [0; 1; 2; 3] in
  let assert_dsw = assert_s_stub 0 in
  let dyn () =
    let dsw = esswitch (S.const 0) switch in
    assert_dsw := vals dsw [2; 3];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) x in
  Gc.full_major ();
  List.iter send_x [0; 1; 2; 3];
  List.iter empty [assert_sw; !assert_dsw ];
  keep_eref create_dyn

let test_switch1 () = (* dynamic creation depends on triggering prim. *)
  let x, set_x = S.create 0 in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun x -> v * x) x in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [9; 12; 15; 18; 21; 24; 27]
    | 1 -> assert_d2 := vals d [36; 42; 48; 54]
    | 2 -> assert_d3 := vals d [81]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 && x <> 0 then Some (dyn x) else None in
  let s = S.switch (S.fmap change x x) in
  let assert_s = vals s [0; 1; 2; 9; 12; 15; 36; 42; 48; 81 ] in
  Gc.full_major ();
  List.iter set_x [1; 1; 2; 3; 3; 4; 5; 6; 6; 7; 8; 9; 9 ];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_esswitch1 () =
  let ex, send_x = E.create () in
  let x = S.hold 0 ex in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun x -> v * x) x in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [9; 12; 15; 18; 21; 24; 27]
    | 1 -> assert_d2 := vals d [36; 42; 48; 54]
    | 2 -> assert_d3 := vals d [81]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 then Some (dyn x) else None in
  let s = esswitch x (E.fmap change (S.changes x)) in
  let assert_s = vals s [0; 1; 2; 9; 12; 15; 36; 42; 48; 81 ] in
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 3; 3; 4; 5; 6; 6; 7; 8; 9; 9 ];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_switch2 () =                           (* test_switch1 + high rank. *)
  let x, set_x = S.create 0 in
  let high_x = high_s x in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun x -> v * x) high_x in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [9; 12; 15; 18; 21; 24; 27]
    | 1 -> assert_d2 := vals d [36; 42; 48; 54]
    | 2 -> assert_d3 := vals d [81]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 && x <> 0 then Some (dyn x) else None in
  let s = S.switch (S.fmap change x x) in
  let assert_s = vals s [0; 1; 2; 9; 12; 15; 36; 42; 48; 81 ] in
  Gc.full_major ();
  List.iter set_x [1; 1; 2; 3; 3; 4; 5; 6; 6; 7; 8; 9; 9 ];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_esswitch2 () =                       (* test_esswitch1 + high rank. *)
  let ex, send_x = E.create () in
  let x = S.hold 0 ex in
  let high_x = high_s x in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun x -> v * x) high_x in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [9; 12; 15; 18; 21; 24; 27]
    | 1 -> assert_d2 := vals d [36; 42; 48; 54]
    | 2 -> assert_d3 := vals d [81]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 then Some (dyn x) else None in
  let s = esswitch x (E.fmap change (S.changes x)) in
  let assert_s = vals s [0; 1; 2; 9; 12; 15; 36; 42; 48; 81 ] in
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_switch3 () = (* dynamic creation does not depend on triggering
                           prim. *)
  let x, set_x = S.create 0 in
  let y, set_y = S.create 0 in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun y -> v * y) y in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [6; 3; 6; 3; 6]
    | 1 -> assert_d2 := vals d [12; 6; 12]
    | 2 -> assert_d3 := vals d [18]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 && x <> 0 then Some (dyn x) else None in
  let s = S.switch (S.fmap change y x) in
  let assert_s = vals s [0; 1; 2; 6; 3; 6; 12; 6; 12; 18] in
  Gc.full_major ();
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [1; 1; 2; 2; 3; 3];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [4; 4; 5; 5; 6; 6];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_esswitch3 () = (* dynamic creation does not depend on triggering
                           prim. *)
  let ex, send_x = E.create () in
  let ey, send_y = E.create () in
  let x = S.hold 0 ex in
  let y = S.hold 0 ey in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun y -> v * y) y in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [6; 3; 6; 3; 6]
    | 1 -> assert_d2 := vals d [12; 6; 12]
    | 2 -> assert_d3 := vals d [18]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 then Some (dyn x) else None in
  let s = esswitch y (E.fmap change (S.changes x)) in
  let assert_s = vals s [0; 1; 2; 6; 3; 6; 12; 6; 12; 18] in
  Gc.full_major ();
  List.iter send_y [1; 1; 2; 2]; List.iter send_x [1; 1; 2; 2; 3; 3];
  List.iter send_y [1; 1; 2; 2]; List.iter send_x [4; 4; 5; 5; 6; 6];
  List.iter send_y [1; 1; 2; 2]; List.iter send_x [7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_switch4 () =                          (* test_switch3 + high rank. *)
  let x, set_x = S.create 0 in
  let y, set_y = S.create 0 in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun y -> v * y) (high_s y) in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [6; 3; 6; 3; 6]
    | 1 -> assert_d2 := vals d [12; 6; 12]
    | 2 -> assert_d3 := vals d [18]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 && x <> 0 then Some (dyn x) else None in
  let s = S.switch (S.fmap change y x) in
  let assert_s = vals s [0; 1; 2; 6; 3; 6; 12; 6; 12; 18] in
  Gc.full_major ();
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [1; 1; 2; 2; 3; 3];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [4; 4; 5; 5; 6; 6];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_esswitch4 () =                       (* test_esswitch3 + high rank. *)
  let ex, set_x = E.create () in
  let ey, set_y = E.create () in
  let x = S.hold 0 ex in
  let y = S.hold 0 ey in
  let dcount = ref 0 in
  let assert_d1 = assert_s_stub 0 in
  let assert_d2 = assert_s_stub 0 in
  let assert_d3 = assert_s_stub 0 in
  let dyn v =
    let d = S.map (fun y -> v * y) (high_s y) in
    begin match !dcount with
    | 0 -> assert_d1 := vals d [6; 3; 6; 3; 6]
    | 1 -> assert_d2 := vals d [12; 6; 12]
    | 2 -> assert_d3 := vals d [18]
    | _ -> assert false
    end;
    incr dcount;
    d
  in
  let change x = if x mod 3 = 0 then Some (dyn x) else None in
  let s = esswitch y (E.fmap change (S.changes x)) in
  let assert_s = vals s [0; 1; 2; 6; 3; 6; 12; 6; 12; 18] in
  Gc.full_major ();
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [1; 1; 2; 2; 3; 3];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [4; 4; 5; 5; 6; 6];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_bind () =
  let e, set_e = E.create () in
  let a = S.hold 0 e in
  let b = S.hold 1 e in
  let s, set_s = S.create true in
  let next = function
  | true -> b
  | false -> a
  in
  let f = S.bind s next in
  let assert_bind = vals f [1; 0; 3;] in
  set_s false;
  set_e 3;
  set_s true;
  List.iter empty [assert_bind]

let test_dyn_bind () = (* i.e. dyn switch *)
  let s1, set_s1 = S.create true in
  let s2, set_s2 = S.create 1 in
  let bind1 = function
  | true ->
      let bind2 = function
      | true -> s2
      | false -> S.const 2
      in
      S.bind s1 bind2
  | false -> S.const 2
  in
  let s = S.bind s1 bind1 in
  let assert_bind = vals s [1; 2; 1 ] in
  set_s1 true;
  set_s1 false;
  set_s1 true;
  List.iter empty [assert_bind]

let test_dyn_bind2 () = (* i.e. dyn switch *)
  let s1, set_s1 = S.create true in
  let s2, set_s2 = S.create true in
  let bind1 = function
  | true ->
      let bind2 = function
      | true -> (S.map (fun _ -> 3) s1)
      | false -> S.const 2
      in
      S.bind s2 bind2
  | false -> S.const 2
  in
  let s = S.bind s1 bind1 in
  let assert_bind = vals s [3; 2; 3 ] in
  set_s1 true;
  set_s1 false;
  set_s1 true;
  List.iter empty [assert_bind]

let test_fix () =
  let s, set_s = S.create 0 in
  let history s =
    let push v = function
      | v' :: _ as l -> if v = v' then l else v :: l
      | [] -> [ v ]
    in
    let define h =
      let h' = S.l2 push s h in
      h', (h', S.map (fun x -> x) h)
    in
    S.fix [] define
  in
  let h, hm = history s in
  let assert_h = vals h [[0]; [1; 0;]; [2; 1; 0;]; [3; 2; 1; 0;]] in
  let assert_hm = vals hm [[0]; [1; 0;]; [2; 1; 0]; [3; 2; 1; 0;]] in
  let assert_dh = assert_s_stub [] in
  let assert_dhm = assert_s_stub [] in
  let assert_dhh = assert_s_stub [] in
  let assert_dhhm = assert_s_stub [] in
  let dyn () =
    let dh, dhm = history s in
    let dhh, dhhm = history (high_s s) in
    assert_dh := vals dh [[1]; [2; 1]; [3; 2; 1]];
    assert_dhm := vals dhm [[]; [1]; [2; 1]; [3; 2; 1]];
    assert_dhh := vals dhh [[1]; [2; 1]; [3; 2; 1]];
    assert_dhhm := vals dhhm [[]; [1]; [2; 1]; [3; 2; 1]];
  in
  let create_dyn = S.map (fun v -> if v = 1 then dyn ()) s in
  Gc.full_major ();
  List.iter set_s [0; 1; 1; 2; 3];
  List.iter empty [assert_h; assert_hm; !assert_dh; !assert_dhm;
                   !assert_dhh; !assert_dhhm];
  keep_sref create_dyn

let test_fix' () =
  let s, set_s = S.create 0 in
  let f, set_f = S.create 3 in
  let hs = high_s s in
  let assert_cs = assert_s_stub 0 in
  let assert_chs = assert_s_stub 0 in
  let assert_cdhs = assert_s_stub 0 in
  let assert_ss = assert_s_stub 0 in
  let assert_shs = assert_s_stub 0 in
  let assert_sdhs = assert_s_stub 0 in
  let assert_fs = assert_s_stub 0 in
  let assert_fhs = assert_s_stub 0 in
  let assert_fdhs = assert_s_stub 0 in
  let dyn () =
    let cs = S.fix 0 (fun h -> S.const 2, S.Int.( + ) h s) in
    let chs = S.fix 0 (fun h -> S.const 2, S.Int.( + ) h hs) in
    let cdhs = S.fix 0 (fun h -> S.const 2, S.Int.( + ) h (high_s s)) in
    let ss = S.fix 0 (fun h -> s, S.Int.( + ) h s) in
    let shs = S.fix 0 (fun h -> s, S.Int.( + ) h hs) in
    let sdhs = S.fix 0 (fun h -> s, S.Int.( + ) h (high_s s)) in
    let fs = S.fix 0 (fun h -> f, S.Int.( + ) h s) in
    let fhs = S.fix 0 (fun h -> f, S.Int.( + ) h hs) in
    let fdhs = S.fix 0 (fun h -> f, S.Int.( + ) h (high_s s)) in
    let cs_vals = [1; 3; 4; 5; ] in
    assert_cs := vals cs cs_vals;
    assert_chs := vals chs cs_vals;
    assert_cdhs := vals cdhs cs_vals;
    let ss_vals = [1; 2; 3; 4; 5; 6] in
    assert_ss := vals ss ss_vals;
    assert_shs := vals shs ss_vals;
    assert_sdhs := vals sdhs ss_vals;
    let fs_vals = [1; 4; 5; 6; 4 ] in
    assert_fs := vals fs fs_vals;
    assert_fhs := vals fhs fs_vals;
    assert_fdhs := vals fdhs fs_vals;
  in
  let create_dyn = S.map (fun v -> if v = 1 then dyn ()) s in
  Gc.full_major ();
  List.iter set_s [0; 1; 1; 2; 3];
  List.iter set_f [1];
  List.iter empty [!assert_cs; !assert_chs; !assert_cdhs;
                   !assert_ss; !assert_shs; !assert_sdhs;
                   !assert_fs; !assert_fhs; !assert_fdhs];
  keep_sref create_dyn

let test_lifters () =
  let f1 a = 1 + a in
  let f2 a0 a1 = a0 + a1 in
  let f3 a0 a1 a2 = a0 + a1 + a2 in
  let f4 a0 a1 a2 a3 = a0 + a1 + a2 + a3 in
  let f5 a0 a1 a2 a3 a4 = a0 + a1 + a2 + a3 + a4 in
  let f6 a0 a1 a2 a3 a4 a5 = a0 + a1 + a2 + a3 + a4 + a5 in
  let x, set_x = S.create 0 in
  let x1 = S.l1 f1 x in
  let x2 = S.l2 f2 x x1 in
  let x3 = S.l3 f3 x x1 x2 in
  let x4 = S.l4 f4 x x1 x2 x3 in
  let x5 = S.l5 f5 x x1 x2 x3 x4 in
  let x6 = S.l6 f6 x x1 x2 x3 x4 x5 in
  let a_x1 = vals x1 [1; 2] in
  let a_x2 = vals x2 [1; 3] in
  let a_x3 = vals x3 [2; 6] in
  let a_x4 = vals x4 [4; 12] in
  let a_x5 = vals x5 [8; 24] in
  let a_x6 = vals x6 [16; 48] in
  let a_dx1 = assert_s_stub 0 in
  let a_dx2 = assert_s_stub 0 in
  let a_dx3 = assert_s_stub 0 in
  let a_dx4 = assert_s_stub 0 in
  let a_dx5 = assert_s_stub 0 in
  let a_dx6 = assert_s_stub 0 in
  let dyn () =
    let dx1 = S.l1 f1 x in
    let dx2 = S.l2 f2 x x1 in
    let dx3 = S.l3 f3 x x1 x2 in
    let dx4 = S.l4 f4 x x1 x2 x3 in
    let dx5 = S.l5 f5 x x1 x2 x3 x4 in
    let dx6 = S.l6 f6 x x1 x2 x3 x4 x5 in
    a_dx1 := vals dx1 [2];
    a_dx2 := vals dx2 [3];
    a_dx3 := vals dx3 [6];
    a_dx4 := vals dx4 [12];
    a_dx5 := vals dx5 [24];
    a_dx6 := vals dx6 [48]
  in
  let create_dyn = S.map (fun v -> if v = 1 then dyn ()) x in
  Gc.full_major ();
  List.iter set_x [0; 1];
  List.iter empty [ a_x1; a_x2; a_x3; a_x4; a_x5; a_x6; !a_dx1; !a_dx2; !a_dx3;
                    !a_dx4; !a_dx5; !a_dx6 ];
  keep_sref create_dyn

let test_option () =
  let b0, set_b0 = S.create None in
  let b1, set_b1 = S.create (Some 1) in
  let b2 = S.const None in
  let b3 = S.const (Some 3) in
  let d, set_d = S.create 512 in
  let dsome = S.Option.some d in
  let s00 = S.Option.value ~default:(`Init (S.const 255)) b0 in
  let s01 = S.Option.value ~default:(`Init (S.const 255)) b1 in
  let s02 = S.Option.value ~default:(`Init (S.const 255)) b2 in
  let s03 = S.Option.value ~default:(`Init (S.const 255)) b3 in
  let s10 = S.Option.value ~default:(`Always (S.const 255)) b0 in
  let s11 = S.Option.value ~default:(`Always (S.const 255)) b1 in
  let s12 = S.Option.value ~default:(`Always (S.const 255)) b2 in
  let s13 = S.Option.value ~default:(`Always (S.const 255)) b3 in
  let s20 = S.Option.value ~default:(`Init d) b0 in
  let s21 = S.Option.value ~default:(`Init d) b1 in
  let s22 = S.Option.value ~default:(`Init d) b2 in
  let s23 = S.Option.value ~default:(`Init d) b3 in
  let s30 = S.Option.value ~default:(`Always d) b0 in
  let s31 = S.Option.value ~default:(`Always d) b1 in
  let s32 = S.Option.value ~default:(`Always d) b2 in
  let s33 = S.Option.value ~default:(`Always d) b3 in
  let a_dsome = vals dsome [ Some 512; Some 1024; Some 2048;] in
  let a_s00 = vals s00 [255;3] in
  let a_s01 = vals s01 [1;] in
  let a_s02 = vals s02 [255;] in
  let a_s03 = vals s03 [3;] in
  let a_s10 = vals s10 [255;3;255] in
  let a_s11 = vals s11 [1;255;] in
  let a_s12 = vals s12 [255] in
  let a_s13 = vals s13 [3] in
  let a_s20 = vals s20 [512;3] in
  let a_s21 = vals s21 [1;] in
  let a_s22 = vals s22 [512] in
  let a_s23 = vals s23 [3] in
  let a_s30 = vals s30 [512;3;1024;2048] in
  let a_s31 = vals s31 [1;512;1024;2048] in
  let a_s32 = vals s32 [512;1024;2048] in
  let a_s33 = vals s33 [3] in
  set_b0 (Some 3); set_b1 None; set_d 1024; set_b0 None; set_d 2048;
  empty a_dsome;
  List.iter empty [ a_s00; a_s01; a_s02; a_s03;
                    a_s10; a_s11; a_s12; a_s13;
                    a_s20; a_s21; a_s22; a_s23;
                    a_s30; a_s31; a_s32; a_s33; ];
  ()

let test_bool () =
  let s, set_s = S.create false in
  let a_zedge = occs (S.Bool.(edge zero)) [] in
  let a_zrise = occs (S.Bool.(rise zero)) [] in
  let a_zfall = occs (S.Bool.(fall zero)) [] in
  let a_sedge = occs (S.Bool.edge s) [true; false] in
  let a_srise = occs (S.Bool.rise s) [()] in
  let a_rfall = occs (S.Bool.fall s) [()] in
  let a_flip_never = vals (S.Bool.flip false E.never) [false] in
  let a_flip = vals (S.Bool.flip true (S.changes s)) [true; false; true] in
  let dyn_flip = S.bind s (fun _ -> S.Bool.flip true (S.changes s)) in
  let a_dyn_flip = vals dyn_flip [true] in
  set_s false; set_s true; set_s true; set_s false;
  List.iter empty [a_zedge; a_sedge; ];
  List.iter empty [a_zrise; a_zfall; a_srise; a_rfall ];
  List.iter empty [a_flip_never; a_flip; a_dyn_flip ];
  ()

let test_signals () =
  test_no_leak ();
  test_hold ();
  test_app ();
  test_map_filter_fmap ();
  test_diff_changes ();
  test_sample ();
  test_on ();
  test_dismiss ();
  test_accum ();
  test_fold ();
  test_merge ();
  test_switch ();
  test_esswitch ();
  test_switch_const ();
  test_esswitch_const ();
  test_switch_const ();
  test_switch1 ();
  test_esswitch1 ();
  test_switch2 ();
  test_esswitch2 ();
  test_switch3 ();
  test_esswitch3 ();
  test_switch4 ();
  test_esswitch4 ();
  test_bind ();
  test_dyn_bind ();
  test_dyn_bind2 ();
  test_fix ();
  test_fix' ();
  test_lifters ();
  test_option ();
  test_bool ();
  ()

(* Test steps *)

let test_executed_raise () =
  let e, send = E.create () in
  let s, set = S.create 4 in
  let step = Step.create () in
  Step.execute step;
  (try send ~step 3; assert false with Invalid_argument _ -> ());
  (try set ~step 3; assert false with Invalid_argument _ -> ());
  (try Step.execute step; assert false with Invalid_argument _ -> ());
  ()

let test_already_scheduled_raise () =
  let e, send = E.create () in
  let s, set = S.create 4 in
  let step = Step.create () in
  let step2 = Step.create () in
  send ~step 3;
  (try send ~step 3; assert false with Invalid_argument _ -> ());
  (try send ~step:step2 4; assert false with Invalid_argument _ -> ());
  set ~step 5;
  set ~step 5; (* doesn't raise because sig value is eq. *)
  (try set ~step 6; assert false with Invalid_argument _ -> ());
  (try set ~step:step2 7; assert false with Invalid_argument _ -> ());
  ()

let test_simultaneous () =
  let e1, send1 = E.create () in
  let e2, send2 = E.create () in
  let s1, set1 = S.create 99 in
  let s2, set2 = S.create 98 in
  let never = E.dismiss e1 e2 in
  let assert_never = occs never [] in
  let merge = E.merge (fun acc o -> o :: acc) [] [e1; e2] in
  let assert_merge = occs merge [[2; 1]] in
  let s1_value = S.sample (fun _ sv -> sv) e1 s1 in
  let assert_s1_value = occs s1_value [ 3 ] in
  let dismiss = S.dismiss e1 1 s1 in
  let assert_dismiss = vals dismiss [ 99 ] in
  let on = S.on (S.map (( = ) 3) s1) 0 s2 in
  let assert_on_ = vals on [0; 4] in
  let step = Step.create () in
  send1 ~step 1;
  send2 ~step 2;
  set1 ~step 3;
  set2 ~step 4;
  Step.execute step;
  empty assert_never;
  empty assert_merge;
  empty assert_s1_value;
  empty assert_dismiss;
  empty assert_on_;
  ()

let test_multistep () =
  let e, send = E.create () in
  let s, set = S.create 0 in
  let assert_e = occs e [1; 2] in
  let assert_s = vals s [0; 1; 2] in
  let step = Step.create () in
  send ~step 1;
  set ~step 1;
  Step.execute step;
  let step = Step.create () in
  send ~step 2;
  set ~step 2;
  Step.execute step;
  empty assert_e;
  empty assert_s;
  ()

let test_steps () =
  test_executed_raise ();
  test_already_scheduled_raise ();
  test_simultaneous ();
  test_multistep ();
  ()

(* bug fixes *)

let test_jake_heap_bug () =
  Gc.full_major ();
  let id x = x in
  let a, set_a = S.create 0 in  (* rank 0 *)
  let _ = S.map (fun x -> if x = 2 then Gc.full_major ()) a in
  let _ =
    let a1 = S.map id a in
    (S.l2 (fun x y -> (x + y)) a1 a), (* rank 2 *)
    (S.l2 (fun x y -> (x + y)) a1 a), (* rank 2 *)
    (S.l2 (fun x y -> (x + y)) a1 a)  (* rank 2 *)
  in
  let _ =
    (S.l2 (fun x y -> (x + y)) a a), (* rank 1 *)
    (S.l2 (fun x y -> (x + y)) a a) (* rank 1 *)
  in
  let d = S.map id (S.map id (S.map (fun x -> x + 1) a)) in (* rank 3 *)
  let h = S.l2 (fun x y -> x + y) a d in (* rank 4 *)
  let a_h = vals h [ 1; 5 ] in
  set_a 2;
  empty a_h

let test_sswitch_init_rank_bug () =
  let enabled, set_enabled = S.create true in
(* let enabled = S.const true *)
  let pos, set_pos = S.create () in
  let down, send_down = E.create () in
  let up, send_up = E.create () in
  let hover enabled = match enabled with
  | true -> S.map (fun a -> true) pos
  | false -> S.Bool.zero
  in
  let used hover enabled = match enabled with
  | true ->
      let start = E.stamp (E.on hover down) true in
      let stop = E.stamp up false in
      let accum = E.select [ start; stop ] in
      let s = S.hold false accum in
      s
  | false -> S.Bool.zero
  in
  let hover = S.bind enabled hover in
  let used = S.switch (S.map ~eq:( == ) (used hover) enabled) in
  let activates = S.changes used in
  let activates' = (E.map (fun _ -> (fun _ -> ())) activates) in
  let actuate = (E.app activates' up) in
  let actuate_assert = occs actuate [()] in
  send_down (); send_up (); empty actuate_assert

let test_changes_end_of_step_add_bug () =
  let s, set_s = S.create false in
  let s1, set_s1 = S.create false in
  let high_s1 = high_s s1 in
  let e = S.changes s1 in
  let assert_o = assert_e_stub () in
  let bind = function
  | true ->
      let changing_rank = S.bind s @@ function
        | true -> high_s1
        | false -> s1
      in
      let o = E.l2 (fun _ _ -> ()) (S.changes changing_rank) e in
      assert_o := occs o [ () ];
      S.const o
  | false -> S.const E.never
  in
  let r = S.bind s bind in
  set_s true;
  set_s1 true;
  List.iter empty [!assert_o;];
  keep_sref r

let test_diff_end_of_step_add_bug () =
  let s, set_s = S.create false in
  let s1, set_s1 = S.create false in
  let high_s1 = high_s s1 in
  let e = S.changes s1 in
  let assert_o = assert_e_stub () in
  let bind = function
  | true ->
      let changing_rank = S.bind s @@ function
        | true -> high_s1
        | false -> s1
      in
      let o = E.l2 (fun _ _ -> ()) (S.diff (fun _ _ -> ()) changing_rank) e in
      assert_o := occs o [ () ];
      S.const o
  | false -> S.const E.never
  in
  let r = S.bind s bind in
  set_s true;
  set_s1 true;
  List.iter empty [!assert_o;];
  keep_sref r

let test_bool_rise_end_of_step_add_bug () =
  let s, set_s = S.create false in
  let s1, set_s1 = S.create false in
  let high_s1 = high_s s1 in
  let e = S.changes s1 in
  let assert_o = assert_e_stub () in
  let bind = function
  | true ->
      let changing_rank = S.bind s @@ function
        | true -> high_s1
        | false -> s1
      in
      let o = E.l2 (fun _ _ -> ()) (S.Bool.rise changing_rank) e in
      assert_o := occs o [ () ];
      S.const o
  | false -> S.const E.never
  in
  let r = S.bind s bind in
  set_s true;
  set_s1 true;
  List.iter empty [!assert_o;];
  keep_sref r

let test_misc () =
  test_jake_heap_bug ();
  test_sswitch_init_rank_bug ();
  test_changes_end_of_step_add_bug ();
  test_diff_end_of_step_add_bug ();
  test_bool_rise_end_of_step_add_bug ();
  ()

let main () =
  test_events ();
  test_signals ();
  test_steps ();
  test_misc ();
  print_endline "All tests succeeded."

let () = main ()

(*----------------------------------------------------------------------------
   Copyright (c) 2009 The react programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
