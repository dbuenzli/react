open B0_kit.V000

(* OCaml library names *)

let react = B0_ocaml.libname "react"
let react_top = B0_ocaml.libname "react.top"

let unix = B0_ocaml.libname "unix"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

(* Libraries *)

let react_lib =
  let srcs = Fpath.[ `File (v "src/react.mli"); `File (v "src/react.ml") ] in
  let requires = [] in
  B0_ocaml.lib react ~doc:"The react library" ~srcs ~requires

let react_top_lib =
  let srcs = Fpath.[ `File (v "src/react_top.ml") ] in
  let requires = [compiler_libs_toplevel] in
  let doc = "The react toplevel support library" in
  B0_ocaml.lib react_top ~doc ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = react :: requires in
  B0_ocaml.exe (Fpath.basename ~strip_ext:true src) ~srcs ~doc ~meta ~requires

let test = test_exe "test/test.ml" ~doc:"Test suite"
let clock =
  test_exe "test/clock.ml" ~doc:"Reactive clock example" ~requires:[unix]

let breakout =
  test_exe "test/breakout.ml" ~doc:"Breakout game example" ~requires:[unix]

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The react programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/react"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/react/doc/"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/react.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/react/issues"
    |> B0_meta.(add description_tags)
      ["reactive"; "declarative"; "signal"; "event"; "frp"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~doc:"react package" ~meta ~locked:true @@
  B0_unit.list ()
