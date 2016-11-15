(* Test for ~strong stop, how to gc a higher-order signal *)

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
let set_counter_ui v =
  counter_ui ##. innerHTML := Js.string (string_of_int v)

let tick, send_tick = E.create ()
let ss =
  S.hold ~eq:( == )
    (S.const 0)
    (E.map (fun v -> S.hold v (high_e tick)) tick)

let gc_ss = S.diff (fun _ old -> S.stop ~strong:true old) ss

let s = S.map (fun v -> set_counter_ui v) (S.switch ss)

let rec loop () =
  incr count;
  send_tick !count;
  ignore (Dom_html.window ## (setTimeout (Js.wrap_callback loop) (1.)))


let main _ = loop (); Js._false

let () = Dom_html.window ##. onload := Dom_html.handler main
