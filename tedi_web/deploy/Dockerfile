# The team instance: `tedi web` over its own `issues/` git store, serving the client bundle.
# Build from the repo root: docker build -f tedi_web/deploy/Dockerfile .
FROM rustlang/rust:nightly-bookworm AS build
RUN rustup target add wasm32-unknown-unknown \
	&& cargo install --locked dioxus-cli@0.7.10 \
	&& curl -fsSL -o /usr/local/bin/tailwindcss https://github.com/tailwindlabs/tailwindcss/releases/download/v4.1.18/tailwindcss-linux-x64 \
	&& chmod +x /usr/local/bin/tailwindcss
WORKDIR /src
COPY . .
ENV RUSTC_WRAPPER=""
RUN cargo build --release --bin tedi \
	&& dx bundle --web --release -p tedi_web --out-dir /out \
	&& cp target/release/tedi /out/tedi

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends git ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=build /out /app
# the store, the config and the personal views live under these
ENV XDG_DATA_HOME=/data XDG_STATE_HOME=/data/state XDG_CACHE_HOME=/data/cache XDG_CONFIG_HOME=/config
VOLUME /data
EXPOSE 8080
ENTRYPOINT ["/app/tedi", "web", "--addr", "0.0.0.0:8080", "--public", "/app/public", "--pull-every", "10m"]
