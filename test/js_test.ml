(* Test for ~strong stop *)

open React

let strong = true

(* Artificially increase memory usage *)
let high_e e =
  let id e = E.map (fun v -> v) e in
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@ id @@
  e

let counter_ui =
  let none () = assert false in
  let el = Dom_html.window ##. document ## (getElementById (Js.string "count")) in
  Js.Opt.get el none

let count = ref 0
let incr_counter () =
  incr count;
  counter_ui ##. innerHTML := Js.string (string_of_int !count)

let tick, send_tick = E.create ()

let rec loop () =
  let ev = E.map (fun () -> incr_counter ()) (high_e tick) in
  send_tick ();
  E.stop ~strong ev;
  ignore (Dom_html.window ## (setTimeout (Js.wrap_callback loop) (1.)))


let main _ = loop (); Js._false

let () = Dom_html.window ##. onload := Dom_html.handler main
