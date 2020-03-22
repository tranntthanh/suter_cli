open Ocamlbuild_plugin

let my_flags () =
  flag ["ocaml"; "pp"; "compile_extend"]
    (S [A"camlp5o"; A "pa_extend.cmo"; A "q_MLast.cmo"]);
  ()

let () = dispatch (function
  | After_rules -> my_flags ()
  | _ -> ())
