FROM ocaml/opam2 as builder
COPY --chown=opam . /home/opam/build
WORKDIR /home/opam/build
RUN make all

FROM debian:latest
COPY --from=builder --chown=root /home/opam/build/wsclient.native /usr/local/bin/suter-cli
RUN apt-get update && apt-get install -y libxxhash0 libgmp10
CMD ["suter-cli"]
