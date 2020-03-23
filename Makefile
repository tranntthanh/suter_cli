wsclient.native: wsclient.ml command.ml connection.ml dsl.ml rt.ml response.ml hash.ml crypto.ml
	ocamlbuild -tags thread -use-ocamlfind wsclient.native

external-deps:
	sudo apt install -y xxhash libxxhash-dev libgmp10 libgmp-dev m4 pkg-config || true

deps: external-deps
	opam pin add tweetnacl https://github.com/vbmithr/ocaml-tweetnacl.git
	opam install ocamlbuild ocamlfind camlp5 yojson stdint digestif websocket lwt lwt_log conduit-lwt-unix cohttp-lwt-unix lwt_ppx hex xxhash tweetnacl

clean:
	ocamlbuild -clean
