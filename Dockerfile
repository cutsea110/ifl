# ============================
# ビルドステージ
# ============================
FROM debian:trixie-slim AS builder

# 必要なツール・ライブラリをインストール
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ghc \
    cabal-install \
    libgmp-dev \
    libffi8 \
    libffi-dev \
    libnuma-dev \
    locales \
    fontconfig \
    fonts-noto-color-emoji \
    && rm -rf /var/lib/apt/lists/*

RUN sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen

WORKDIR /build

COPY . .

RUN cabal update
RUN cabal install exe:ifl --installdir=/build/dist --overwrite-policy=always --enable-executable-dynamic

# ============================
# 実行ステージ（distroless）
# ============================
FROM gcr.io/distroless/cc-debian13

COPY --from=builder /build/dist/ifl /ifl
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgmp.so.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libffi.so.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libnuma.so.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libfontconfig.so.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libfreetype.so.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libpng16.so.* /usr/lib/x86_64-linux-gnu/

COPY --from=builder /usr/lib/locale /usr/lib/locale
COPY --from=builder /usr/share/locale /usr/share/locale

COPY --from=builder /usr/share/fonts /usr/share/fonts
COPY --from=builder /etc/fonts /etc/fonts

ENV FONTCONFIG_PATH=/etc/fonts
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

ENTRYPOINT ["/ifl"]
