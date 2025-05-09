FROM haskell:latest AS builder

WORKDIR /work/
COPY . /work/
RUN cabal update
RUN cabal build
RUN cabal install --installdir=./ --install-method=copy

FROM debian:sid-slim

COPY --from=builder /work/ifl /usr/bin/ifl
CMD ["ifl"]
