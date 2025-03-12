FROM ghcr.io/saethlin/crates-build-env:latest@sha256:ad50a1e5d776d7621862f811f9374e02ae9b9f49ca033adcc1741aff6a97ab63

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
COPY .gitconfig /root/.gitconfig

ADD "https://api.github.com/repos/JoJoDeveloping/miri/commits?per_page=1&sha=miri-for-test-run" latest_commit
RUN cd /tmp && git clone https://github.com/JoJoDeveloping/miri.git && cd miri && \
    git checkout aea2a31494c7d21625d44145fdb582763dae4bfc && ./miri toolchain && ./miri install
    

COPY run.sh /root/run.sh

RUN mkdir /build /output
RUN rm -rf /root/.cache
RUN for target in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu; do \
        echo "tb-sb-differences-canary@0.2.0" | TOOL=miri TARGET=$target TEST_END_DELIMITER="-" bash /root/run.sh; \
    done
RUN tar czvf /cache.tar.gz /root/.cache
RUN rm -rf /root/.cache /build
RUN rm -rf /output

RUN mkdir /output

ENTRYPOINT ["/usr/bin/nice", "-n19", "bash", "/root/run.sh"]
