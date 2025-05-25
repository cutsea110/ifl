FROM haskell:latest AS builder

WORKDIR /work/
COPY . /work/
RUN cabal update
RUN cabal build
RUN cabal install --installdir=./ --install-method=copy

FROM debian:sid-slim

# Add Locale settings
RUN apt-get update && \
    apt-get install -y locales && \
    sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

COPY --from=builder /work/ifl /usr/bin/ifl
CMD ["ifl"]
