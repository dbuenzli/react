#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo_test ~cond test =
  Pkg.flatten
    [ Pkg.test ~run:false ~cond ~auto:false (test ^ ".js");
      Pkg.test ~run:false ~cond ~auto:false (test ^ ".html"); ]

let () =
  Pkg.describe "react" @@ fun c ->
  Ok [ Pkg.mllib "src/react.mllib";
       Pkg.mllib ~api:[] "src/react_top.mllib";
       Pkg.lib "src/react_top_init.ml";
       Pkg.test ~run:false "test/breakout";
       Pkg.test ~run:false "test/clock";
       Pkg.test "test/test";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    (* jsoo_test ~cond:jsoo "test/js_hisig_test";
       jsoo_test ~cond:jsoo "test/js_test"; *)
     ]
