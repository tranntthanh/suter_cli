OCB_FLAGS = -use-ocamlfind -tags thread -tags bin_annot
OCB_STATIC_FLAGS = -tags 'cclib(-static)'
OCB = opam config exec -- ocamlbuild
SRC_FILES = wsclient.ml command.ml connection.ml dsl.ml rt.ml response.ml hash.ml crypto.ml encode.ml

wsclient.native: $(SRC_FILES)
	$(OCB) $(OCB_FLAGS) wsclient.native

static-wsclient.native: $(SRC_FILES)
	$(OCB) $(OCB_FLAGS) $(OCB_STATIC_FLAGS) wsclient.native

external-deps:
	sudo apt-get update || true
	sudo apt-get install -y xxhash libxxhash-dev libgmp10 libgmp-dev m4 pkg-config || true

deps: external-deps
	opam update
	opam pin add tweetnacl https://github.com/vbmithr/ocaml-tweetnacl.git
	opam install ocamlbuild ocamlfind tls camlp5 yojson stdint digestif websocket lwt lwt_log conduit-lwt-unix cohttp-lwt-unix lwt_ppx hex xxhash tweetnacl

all: deps wsclient.native

static-all: deps static-wsclient.native

clean:
	$(OCB) -clean
