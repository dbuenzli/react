(* This code is in the public domain.
   Prints a clock with the current local time in the terminal.  *)

let pr_time t =
  let tm = Unix.localtime t in
  Printf.printf "\x1B[8D%02d:%02d:%02d%!"
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

open React;;

let seconds, run =
  let e, send = E.create () in
  let run () = while true do send (Unix.gettimeofday ()); Unix.sleep 1 done in
  e, run

let printer = E.map pr_time seconds

let () = run ()
