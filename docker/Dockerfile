FROM ghcr.io/saethlin/crates-build-env:latest

ENV PATH=/root/.cargo/bin:$PATH

RUN apt-get update && apt-get install -y curl build-essential && \
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
    sh -s -- --default-toolchain=nightly --component=miri --component=rust-src --profile=minimal -y && \
    cargo install --git https://github.com/saethlin/miri-tools cargo-download inapty get-args && \
    cargo install --git https://github.com/RalfJung/cargo-careful cargo-careful && \
    curl -L https://get.nexte.st/latest/linux | tar zxf - && mv cargo-nextest /root/.cargo/bin/ && \
    rm -rf /var/lib/apt/lists/*

RUN cargo install rustup-toolchain-install-master

COPY nextest.toml /root/.cargo/nextest.toml
COPY run.sh /root/run.sh
COPY .gitconfig /root/.gitconfig

RUN cd /tmp && git clone https://github.com/JoJoDeveloping/miri.git -b miri-for-test-run && cd miri && \
    ./miri toolchain && ./miri install
    

RUN mkdir /build /output
RUN rm -rf /root/.cache
RUN for target in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu; do \
        echo "tb-sb-differences-canary@0.1.0" | TOOL=miri TARGET=$target TEST_END_DELIMITER="-" bash /root/run.sh; \
    done
RUN tar czvf /cache.tar.gz /root/.cache
RUN rm -rf /root/.cache /build
RUN rm -rf /output

RUN mkdir /output

ENTRYPOINT ["/usr/bin/nice", "-n19", "bash", "/root/run.sh"]
