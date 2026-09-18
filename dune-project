(lang dune 3.23)

(name dolphinc)

(generate_opam_files true)

(source
 (github GioMaz/dolphinc))

(package
 (name dolphinc)
 (synopsis "Dolphin compiler")
 (depends
   (ocaml (= "5.2.1"))
   (yojson (= "3.0.0"))
   (cmdliner (= "2.1.1"))
   (ppx_yojson_conv_lib (= "v0.17.0")))
 )
