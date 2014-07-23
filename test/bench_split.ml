open React

let n_ev_r = ref 1000
let n_it_r = ref 1000

let test_filter () =
  let ev, set_ev = E.create () in
  let a = Array.init !n_ev_r (fun i -> E.filter (fun (i', _) -> i' = i) ev) in

  let r = ref (-1, -1, -1) in
  for i = 0 to !n_ev_r - 1 do
    a.(i) <- E.trace (fun (i', k) -> r := (i, i', k)) a.(i)
  done;

  for k = 0 to !n_it_r - 1 do
    let i = Random.int !n_ev_r in
    set_ev (i, k);
    assert (!r = (i, i, k))
  done;
  a

let test_split () =
  let ev, set_ev = E.create () in
  let get_ev = E.split fst ev in
  let a = Array.init !n_ev_r get_ev in

  let r = ref (-1, -1, -1) in
  for i = 0 to !n_ev_r - 1 do
    a.(i) <- E.trace (fun (i', k) -> r := (i, i', k)) a.(i)
  done;

  for k = 0 to !n_it_r - 1 do
    let i = Random.int !n_ev_r in
    set_ev (i, k);
    assert (!r = (i, i, k))
  done;
  a

let benchmark name f =
  Gc.full_major ();
  let tmsI = Unix.times () in
  ignore (f ());
  let tmsF = Unix.times () in
  let dt = Unix.(tmsF.tms_utime -. tmsI.tms_utime) in
  Printf.printf "%s: %g\n" name dt

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then n_ev_r := int_of_string Sys.argv.(1);
  if argc > 2 then n_it_r := int_of_string Sys.argv.(2);
  benchmark "filter" test_filter;
  benchmark "split" test_split
