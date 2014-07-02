#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let () = 
  Pkg.describe "react" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/react";
    Pkg.lib ~exts:Exts.library "src/react_top";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/breakout.ml";
    Pkg.doc "test/clock.ml"; ]



