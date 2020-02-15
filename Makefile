wsclient.native: wsclient.ml command.ml connection.ml dsl.ml rt.ml response.ml hash.ml crypto.ml
	ocamlbuild -tags thread -use-ocamlfind wsclient.native

clean:
	ocamlbuild -clean
