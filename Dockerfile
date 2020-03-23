FROM ocaml/opam2 as builder
COPY . /home/opam/build
WORKDIR /home/opam/build
RUN eval "$(opam env)" && make deps && make

FROM phusion/baseimage:0.11
COPY --from=builder /home/opam/build/wsclient.native /usr/local/bin/suter_cli
RUN apt update && apt install -y xxhash libgmp10
CMD ["suter_cli"]
