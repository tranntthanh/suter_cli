FROM ocaml/opam2:alpine as builder
RUN sudo apk add gmp gmp-dev perl m4 pkgconfig
# xxhash from apk add libxxhash does not contain libxxhash.a
# https://github.com/alpinelinux/aports/blob/64943b66778c0fde88112d7e63da88efdc210416/community/xxhash/PARBOILED#L47
WORKDIR /home/opam
RUN VERSION=0.7.3 && \
    curl -LO https://github.com/Cyan4973/xxHash/archive/v$VERSION.tar.gz && \
    tar xaf v$VERSION.tar.gz && cd xxHash-$VERSION && \
    make && sudo make install
COPY --chown=opam . /home/opam/build
WORKDIR /home/opam/build
RUN make static-all

FROM apline:latest
COPY --from=builder --chown=root /home/opam/build/wsclient.native /usr/local/bin/suter-cli
ENTRYPOINT ["suter-cli"]
