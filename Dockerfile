FROM haskell:latest AS builder

WORKDIR /work/
COPY . /work/
RUN cabal build
RUN cabal install --installdir=./ --install-method=copy

FROM debian:latest

COPY --from=builder /work/ifl /usr/bin/ifl
CMD ["ifl"]
