# ============================
# ビルドステージ
# ============================
FROM debian:trixie-slim AS builder
ENV DEBIAN_FRONTEND=noninteractive

# 基本ツール + TLS + Haskellビルドにありがちな依存
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    ca-certificates \
    git \
    pkg-config \
    zlib1g-dev \
    libgmp-dev \
    libffi-dev \
    libnuma-dev \
    libtinfo-dev \
    locales \
    && rm -rf /var/lib/apt/lists/*

RUN update-ca-certificates

# locale（必要なら）
RUN sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen

# ghcup で GHC/Cabal を固定
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV GHCUP_INSTALL_BASE_PREFIX=/opt

RUN set -eux; \
    curl -fsSL https://get-ghcup.haskell.org -o /tmp/ghcup.sh; \
    sh /tmp/ghcup.sh

ENV PATH=/opt/.ghcup/bin:/opt/.cabal/bin:$PATH

RUN ghcup install ghc 9.14.1 && ghcup set ghc 9.14.1
RUN ghcup install cabal 3.14.2.0 && ghcup set cabal 3.14.2.0
RUN ghc --version && cabal --version

WORKDIR /build
COPY . .

# ビルド
RUN set -eux; \
    cabal update; \
    mkdir -p /build/dist; \
    cabal install exe:ifl \
      --installdir=/build/dist \
      --overwrite-policy=always

# サイズ削減
RUN strip /build/dist/ifl || true

# runtime に持っていく共有ライブラリを収集（distroless で不足しやすいものだけ）
# - libc/ld-linux/libm は distroless/cc 側にある前提で除外
RUN set -eux; \
    mkdir -p /build/runtime-libs; \
    ldd /build/dist/ifl \
      | awk '/=> \// {print $3} /^\/lib/ {print $1}' \
      | sort -u \
      | grep -vE '/libc\.so\.6$|/ld-linux|/libm\.so\.6$' \
      | xargs -r -I{} cp -v {} /build/runtime-libs/; \
    echo "=== verify (builder) ==="; \
    LD_LIBRARY_PATH=/build/runtime-libs ldd /build/dist/ifl || true

# ============================
# 実行ステージ（distroless）
# ============================
FROM gcr.io/distroless/cc-debian13

COPY --from=builder /build/dist/ifl /ifl
COPY --from=builder /build/runtime-libs/ /usr/local/lib/

# 収集した .so を見せる
ENV LD_LIBRARY_PATH=/usr/local/lib

# locale が必要なら（不要なら削除してサイズ削減）
COPY --from=builder /usr/lib/locale /usr/lib/locale
COPY --from=builder /usr/share/locale /usr/share/locale
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

ENTRYPOINT ["/ifl"]
