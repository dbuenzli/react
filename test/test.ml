(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ----------------------------------------------------------------------------*)

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

let test_when () = 
  let e, send_e = E.create () in 
  let s = S.hold 0 e in 
  let c = S.map (fun x -> x mod 2 = 0) s in
  let w = E.when_ c e in 
  let ovals = [2; 4; 4; 6; 4] in
  let assert_w = occs w ovals in
  let assert_dw = assert_e_stub () in
  let assert_dhw = assert_e_stub () in
  let dyn () = 
    let dw = E.when_ c e in 
    let dhw = E.when_ (high_s c) (high_e e) in
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

let test_events () = 
  test_no_leak ();
  test_once_drop_once ();
  test_app ();
  test_map_stamp_filter_fmap ();
  test_diff_changes ();
  test_when ();
  test_dismiss ();
  test_until ();
  test_accum ();
  test_fold ();
  test_select ();
  test_merge ();
  test_switch ();
  test_fix ()

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

let test_when () =
  let s, set_s = S.create 0 in 
  let ce = S.map (fun x -> x mod 2 = 0) s in  
  let co = S.map (fun x -> x mod 2 <> 0) s in
  let se = S.when_ ce 42 s in 
  let so = S.when_ co 56 s in 
  let assert_se = vals se [ 0; 2; 4; 6; 4 ] in
  let assert_so = vals so [ 56; 1; 3; 1; 3 ] in
  let assert_dse = assert_s_stub 0 in
  let assert_dhse = assert_s_stub 0 in
  let assert_dso = assert_s_stub 0 in
  let assert_dhso = assert_s_stub 0 in
  let dyn () = 
    let dse = S.when_ ce 42 s in
    let dhse = S.when_ ce 42 (high_s s) in
    let dso = S.when_ co 56 s in
    let dhso = S.when_ co 56 (high_s s) in
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

let test_switch () =
  let x, send_x = E.create () in 
  let s = S.hold 0 x in
  let switch s = 
    E.fmap (fun v -> if v mod 3 = 0 then Some (S.map (( * ) v) s) else None) x
  in
  let sw = S.switch s (switch s) in
  let hsw = S.switch s (switch (high_s s)) in
  let assert_sw = vals sw [0; 1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_hsw = vals hsw [0; 1; 2; 9; 12; 15; 36; 42; 48; 81] in
  let assert_dsw = assert_s_stub 0 in
  let assert_dhsw = assert_s_stub 0 in
  let dyn () = 
    let dsw = S.switch s (switch s) in 
    let dhsw = S.switch s (switch (high_s s)) in
    assert_dsw := vals dsw [9; 12; 15; 36; 42; 48; 81];
    assert_dhsw := vals dhsw [9; 12; 15; 36; 42; 48; 81];
  in
  let create_dyn = E.map (fun v -> if v = 3 then dyn ()) x in 
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 2; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9];
  List.iter empty [assert_sw; assert_hsw; !assert_dsw; !assert_dhsw];
  keep_eref create_dyn

let test_switch_const () = 
  let x, send_x = E.create () in 
  let switch = E.map (fun x -> S.const x) x in
  let sw = S.switch (S.const 0) switch in
  let assert_sw = vals sw [0; 1; 2; 3] in
  let assert_dsw = assert_s_stub 0 in 
  let dyn () = 
    let dsw = S.switch (S.const 0) switch in 
    assert_dsw := vals dsw [2; 3];
  in
  let create_dyn = E.map (fun v -> if v = 2 then dyn ()) x in 
  Gc.full_major ();
  List.iter send_x [0; 1; 2; 3];
  List.iter empty [assert_sw; !assert_dsw ];
  keep_eref create_dyn
      
let test_switch1 () =         (* dynamic creation depends on triggering prim. *)
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
  let s = S.switch x (E.fmap change (S.changes x)) in 
  let assert_s = vals s [0; 1; 2; 9; 12; 15; 36; 42; 48; 81 ] in
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 3; 3; 4; 5; 6; 6; 7; 8; 9; 9 ];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_switch2 () =                           (* test_switch1 + high rank. *)
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
  let s = S.switch x (E.fmap change (S.changes x)) in 
  let assert_s = vals s [0; 1; 2; 9; 12; 15; 36; 42; 48; 81 ] in
  Gc.full_major ();
  List.iter send_x [1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_switch3 () = (* dynamic creation does not depend on triggering prim. *)
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
  let s = S.switch y (E.fmap change (S.changes x)) in
  let assert_s = vals s [0; 1; 2; 6; 3; 6; 12; 6; 12; 18] in
  Gc.full_major ();
  List.iter send_y [1; 1; 2; 2]; List.iter send_x [1; 1; 2; 2; 3; 3];
  List.iter send_y [1; 1; 2; 2]; List.iter send_x [4; 4; 5; 5; 6; 6];
  List.iter send_y [1; 1; 2; 2]; List.iter send_x [7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

let test_switch4 () =                          (* test_switch3 + high rank. *)
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
  let s = S.switch y (E.fmap change (S.changes x)) in
  let assert_s = vals s [0; 1; 2; 6; 3; 6; 12; 6; 12; 18] in
  Gc.full_major ();
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [1; 1; 2; 2; 3; 3];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [4; 4; 5; 5; 6; 6];
  List.iter set_y [1; 1; 2; 2]; List.iter set_x [7; 7; 8; 8; 9; 9];
  List.iter empty [assert_s; !assert_d1; !assert_d2; !assert_d3]

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

let test_signals () =   
  test_no_leak ();
  test_hold ();
  test_app ();
  test_map_filter_fmap ();
  test_diff_changes ();
  test_sample ();
  test_when ();
  test_dismiss ();
  test_accum ();
  test_fold ();
  test_merge ();
  test_switch ();
  test_switch_const ();
  test_switch1 ();
  test_switch2 ();
  test_switch3 (); 
  test_switch4 ();
  test_fix ();
  test_fix' ();
  test_lifters ();
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


let test_misc () = test_jake_heap_bug ()
  
let main () = 
  test_events ();
  test_signals ();
  test_misc ();
  print_endline "All tests succeeded."

let () = main ()

(*----------------------------------------------------------------------------
  Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the Daniel C. Bünzli nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)



